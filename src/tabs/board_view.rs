use std::{
    collections::{BTreeMap, HashSet},
    f32::consts::{PI, SQRT_2, TAU},
    num::NonZeroU32,
    ops::{Deref, Not},
    sync::Arc,
};

use eframe::{
    egui::{
        remap_clamp, vec2, Align, Color32, FontId, Key, PointerButton, Rect, Response, Rounding,
        Sense, Stroke, TextStyle, Ui, WidgetText,
    },
    epaint::TextShape,
};
use parking_lot::{Mutex, RwLock};

use crate::{
    app::{App, SelectedItem},
    circuits::{CircuitBlueprint, CircuitRenderingContext, CircuitSelectionRenderingContext},
    drawing::rotated_rect,
    editor::{BoardEditor, BoardSelectionImpl, QuarterPos, SelectedBoardItem},
    selection::{Selection, SelectionRenderer},
    vector::{Vec2f, Vec2isize, Vector2},
    vertex_renderer::{ColoredLineBuffer, ColoredTriangleBuffer, ColoredVertexRenderer},
    CustomPaintContext, Direction4Half, Direction8, Direction8Array, PaintContext, Screen,
    BIG_WIRE_POINT_WIDTH, CHUNK_SIZE, WIRE_POINT_WIDTH, WIRE_WIDTH,
};

use super::{TabCreation, TabImpl};

pub struct BoardView {
    pan_zoom: PanAndZoom,

    vertexes: Arc<ColoredVertexRenderer>,

    wire_draw_start: Option<Vec2isize>,

    editor: Arc<RwLock<BoardEditor>>,
    fixed_screen_pos: Option<(Rect, PanAndZoom)>,
    wire_debug: bool,
    circuit_debug: bool,

    grid_buffer: Arc<Mutex<ColoredLineBuffer>>,
    wire_part_buffer: Arc<Mutex<ColoredTriangleBuffer>>,
    pin_buffer: Arc<Mutex<ColoredTriangleBuffer>>,

    selection: Selection<BoardSelectionImpl>,
    selection_renderer: Arc<Mutex<SelectionRenderer>>,

    circuits_drawn: HashSet<usize>,
    wire_colors: BTreeMap<usize, Color32>,
    debug_not_render_wires: bool,
}

impl TabCreation for BoardView {
    fn new(app: &App) -> Self {
        Self {
            pan_zoom: Default::default(),
            vertexes: Arc::new(ColoredVertexRenderer::new(&app.gl).expect("initialized renderer")),
            wire_draw_start: None,

            editor: Default::default(),
            fixed_screen_pos: None,
            wire_debug: false,
            circuit_debug: false,

            grid_buffer: Arc::new(Mutex::new(ColoredLineBuffer::new(1.0))),
            wire_part_buffer: Default::default(),
            pin_buffer: Default::default(),

            selection: Selection::new(),
            selection_renderer: Arc::new(Mutex::new(SelectionRenderer::new(&app.gl))),

            circuits_drawn: HashSet::new(),
            wire_colors: BTreeMap::new(),
            debug_not_render_wires: false,
        }
    }
}

impl TabImpl for BoardView {
    fn update(&mut self, app: &mut App, ui: &mut Ui) {
        let screen_rect = ui.max_rect();
        let interaction = ui.interact(
            screen_rect,
            ui.id().with("board-interaction"),
            Sense::click_and_drag(),
        );
        if self.fixed_screen_pos.is_none() || ui.input(|input| input.modifiers.alt) {
            self.pan_zoom.update(
                ui,
                screen_rect,
                interaction.dragged_by(PointerButton::Secondary),
                &interaction,
            );
        }

        let global_screen = self.pan_zoom.to_screen(screen_rect);

        if ui.input(|input| input.key_pressed(Key::F5)) {
            match self.fixed_screen_pos.is_none() {
                true => {
                    let rect = Rect::from_min_max(
                        global_screen.screen_to_world(0.0).into(),
                        global_screen.screen_to_world(screen_rect.size()).into(),
                    );
                    self.fixed_screen_pos =
                        Some((rect, PanAndZoom::new(rect.center().into(), 1.0)));
                    let mul = (screen_rect.width() - 40.0) / screen_rect.width();
                    self.pan_zoom.scale *= mul;
                }
                false => {
                    self.fixed_screen_pos = None;
                }
            }
        }

        let screen = match &mut self.fixed_screen_pos {
            None => global_screen,
            Some((rect, pan_zoom)) => {
                pan_zoom.scale *= self.pan_zoom.scale;
                let screen_rect = global_screen.world_to_screen_rect(*rect);
                if !ui.input(|input| input.modifiers.alt) {
                    pan_zoom.update(
                        ui,
                        screen_rect,
                        interaction.dragged_by(PointerButton::Secondary),
                        &interaction,
                    );
                }
                let screen = pan_zoom.to_screen(screen_rect);
                pan_zoom.scale /= self.pan_zoom.scale;
                screen
            }
        };

        if self.fixed_screen_pos.is_some() {
            ui.painter().rect_stroke(
                screen.screen_rect,
                Rounding::ZERO,
                Stroke::new(1.0, Color32::RED),
            );
        }

        self.selection.update(
            self.editor.read().deref(),
            &interaction,
            ui,
            screen,
            matches!(app.selected_item, Some(SelectedItem::Selection)),
        );

        if !ui.ctx().wants_keyboard_input() {
            self.handle_keyboard(ui, app);
        }

        let ctx = PaintContext::new(ui, screen, app.style.clone());

        self.draw_grid(&ctx);

        self.selection_renderer.lock().clear_draw();

        self.prepare_wire_draw(&ctx);
        self.draw_selection(&ctx);

        if !self.debug_not_render_wires {
            self.draw_wires(&ctx);
        }
        self.draw_pins(&ctx);

        self.draw_circuits(&ctx);

        if self.wire_debug {
            self.draw_wire_debug(&ctx);
        }

        if self.circuit_debug {
            self.draw_circuit_debug(&ctx);
        }

        self.selection.draw_overlay(&ctx);

        self.handle_wire_interactions(
            &ctx,
            &interaction,
            matches!(app.selected_item, Some(SelectedItem::Wires)),
        );

        if let Some(SelectedItem::Circuit(c)) = app.selected_item.as_ref() {
            self.handle_circuit_placement(&interaction, &ctx, c.read().deref());
        }
    }

    fn tab_style_override(&self, global: &egui_dock::TabStyle) -> Option<egui_dock::TabStyle> {
        let mut style = global.clone();
        style.tab_body.inner_margin = 2.0.into();
        Some(style)
    }
}

impl BoardView {
    fn draw_grid_layer(ctx: &CustomPaintContext, buffer: &mut ColoredLineBuffer, chunks: bool) {
        let (scale_mul, fade_min, fade_max) = if chunks {
            (
                CHUNK_SIZE as f32,
                CHUNK_SIZE as f32 / 8.0,
                CHUNK_SIZE as f32 / 2.0,
            )
        } else {
            (1.0, CHUNK_SIZE as f32 / 4.0, CHUNK_SIZE as f32)
        };

        let scale = ctx.screen.scale * scale_mul;

        if ctx.screen.scale > fade_min {
            let alpha = remap_clamp(ctx.screen.scale, fade_min..=fade_max, 0.0..=1.0);

            let offx = (1.0 - ((ctx.screen.world_pos.x / scale_mul).fract() + 1.0).fract()) * scale;
            let offy = (1.0 - ((ctx.screen.world_pos.y / scale_mul).fract() + 1.0).fract()) * scale;

            let countx = (ctx.screen.screen_rect.width() / scale).ceil() as usize;
            let county = (ctx.screen.screen_rect.height() / scale).ceil() as usize;

            let white = if chunks { 0.2 } else { 0.15 };

            for i in 0..countx {
                let x = offx + i as f32 * scale + ctx.screen.screen_rect.left();
                let wx = i as isize + (ctx.screen.world_pos.x / scale_mul).floor() as isize + 1;

                if (wx % CHUNK_SIZE as isize) == 0 && !chunks || wx == 0 {
                    continue;
                }

                buffer.add_singlecolor_line(
                    [x, ctx.screen.screen_rect.top()],
                    [x, ctx.screen.screen_rect.bottom()],
                    [white * alpha, white * alpha, white * alpha, alpha],
                );
            }

            for i in 0..county {
                let y = offy + i as f32 * scale + ctx.screen.screen_rect.top();
                let wy = i as isize + (ctx.screen.world_pos.y / scale_mul).floor() as isize + 1;

                if (wy % CHUNK_SIZE as isize) == 0 && !chunks || wy == 0 {
                    continue;
                }

                buffer.add_singlecolor_line(
                    [ctx.screen.screen_rect.left(), y],
                    [ctx.screen.screen_rect.right(), y],
                    [white * alpha, white * alpha, white * alpha, alpha],
                );
            }
        }

        let zero = ctx.screen.world_to_screen(0.0);

        if ctx.screen.screen_rect.left() <= zero.x && zero.x < ctx.screen.screen_rect.right() {
            buffer.add_singlecolor_line(
                [zero.x, ctx.screen.screen_rect.top()],
                [zero.x, ctx.screen.screen_rect.bottom()],
                [0.2, 1.0, 0.2, 1.0],
            );
        }

        if ctx.screen.screen_rect.top() <= zero.y && zero.y < ctx.screen.screen_rect.bottom() {
            buffer.add_singlecolor_line(
                [ctx.screen.screen_rect.left(), zero.y],
                [ctx.screen.screen_rect.right(), zero.y],
                [1.0, 0.2, 0.2, 1.0],
            );
        }
    }

    fn draw_grid_cross(ctx: &CustomPaintContext, buffer: &mut ColoredLineBuffer) {
        let cross_pos = ctx.screen.world_to_screen(0.0);
        let cross_pos = ctx.screen.screen_rect.clamp(cross_pos.into());

        buffer.add_singlecolor_line(
            [cross_pos.x - ctx.screen.scale, cross_pos.y],
            [cross_pos.x + ctx.screen.scale, cross_pos.y],
            [1.0; 4],
        );

        buffer.add_singlecolor_line(
            [cross_pos.x, cross_pos.y - ctx.screen.scale],
            [cross_pos.x, cross_pos.y + ctx.screen.scale],
            [1.0; 4],
        );
    }

    fn draw_grid(&self, ctx: &PaintContext) {
        let buffer = self.grid_buffer.clone();
        let vertexes = self.vertexes.clone();

        ctx.custom_draw(move |ctx| {
            let mut buffer = buffer.lock();
            buffer.clear();

            Self::draw_grid_layer(&ctx, &mut buffer, false);
            Self::draw_grid_layer(&ctx, &mut buffer, true);
            Self::draw_grid_cross(&ctx, &mut buffer);

            vertexes.draw(
                ctx.painter.gl(),
                ctx.paint_info.screen_size_px,
                buffer.deref(),
            );
        });
    }

    fn prepare_wire_draw(&mut self, ctx: &PaintContext) {
        fn node_has_135_deg_turns(dirs: &Direction8Array<Option<NonZeroU32>>) -> bool {
            for (dir, dist) in dirs.iter() {
                if dist.is_none() {
                    continue;
                }

                if dirs
                    .get(dir.rotated_clockwise_by(Direction8::DownRight))
                    .is_some()
                    || dirs
                        .get(dir.rotated_counterclockwise_by(Direction8::DownRight))
                        .is_some()
                {
                    return true;
                }
            }
            false
        }

        fn node_is_a_45_deg_turn(dirs: &Direction8Array<Option<NonZeroU32>>) -> bool {
            let mut iter = dirs.iter();

            for (dir, dist) in &mut iter {
                if dist.is_none() {
                    continue;
                }

                if dirs
                    .get(dir.rotated_clockwise_by(Direction8::UpRight))
                    .is_some()
                    || dirs
                        .get(dir.rotated_counterclockwise_by(Direction8::UpRight))
                        .is_some()
                {
                    break;
                } else {
                    return false;
                }
            }

            let mut one = false;

            for (_, d) in iter {
                if d.is_none() {
                    continue;
                }

                match one {
                    false => one = true,
                    true => return false,
                }
            }

            true
        }

        fn part_add_len(dirs: &Direction8Array<Option<NonZeroU32>>, dir: Direction8) -> f32 {
            // Arrow shape with `dir` being the middle
            if dirs
                .get(dir.rotated_clockwise_by(Direction8::UpRight))
                .is_some()
                && dirs
                    .get(dir.rotated_counterclockwise_by(Direction8::UpRight))
                    .is_some()
            {
                0.0
            } else if node_has_135_deg_turns(dirs) {
                0.5 * (22.5f32).to_radians().tan()
            } else if node_is_a_45_deg_turn(dirs) {
                if dir.is_diagonal() {
                    0.5 * -(22.5f32).to_radians().tan()
                } else {
                    0.5 * (22.5f32).to_radians().tan()
                }
            } else {
                0.5
            }
        }

        let mut part_buffer = self.wire_part_buffer.lock();
        let mut selecion_renderer = self.selection_renderer.lock();
        let editor = self.editor.read();

        part_buffer.clear();
        self.wire_colors.clear();

        let tl = [ctx.tile_bounds_tl.x - 1, ctx.tile_bounds_tl.y].into();
        let size = [ctx.tile_bounds_size.x + 2, ctx.tile_bounds_size.y + 1].into();

        let force_draw_points = ctx.ui.input(|input| input.modifiers.shift);

        for (pos, lookaround, node) in editor.wires.iter_area_with_lookaround(tl, size) {
            for dir in Direction4Half::ALL {
                let dist = node.directions.get(dir.into());
                let Some(dist) = dist else {
                    continue;
                };

                let target_rel =
                    Direction8::from_half(dir, false).into_dir_isize() * dist.get() as isize;
                let target_node = lookaround.get_relative(target_rel);

                let (wire, part_pos) = match &node.wire {
                    Some(wire) => (wire, pos),
                    None => {
                        match dir {
                            Direction4Half::Left if pos.x > ctx.tile_bounds_br.x => (),
                            Direction4Half::UpLeft
                                if pos.y > ctx.tile_bounds_br.y || pos.x > ctx.tile_bounds_br.x => {
                            }
                            Direction4Half::Up if pos.y > ctx.tile_bounds_br.y => {}
                            Direction4Half::UpRight
                                if pos.y > ctx.tile_bounds_br.y || pos.x < ctx.tile_bounds_tl.x => {
                            }
                            _ => continue,
                        }
                        let wire = target_node.and_then(|n| n.wire.as_ref());
                        let Some(wire) = wire else {
                            continue;
                        };

                        let dir = Direction8::from_half(dir, true);
                        let dist = node.directions.get(dir);
                        let pos = pos
                            + dir.into_dir_isize() * dist.map(|d| d.get() as isize).unwrap_or(0);

                        (wire, pos)
                    }
                };

                let len = dist.get() as f32;

                let full_dir = Direction8::from_half(dir, false);

                let angle = full_dir.into_angle_xp_cw();

                let this_add_len = part_add_len(&node.directions, full_dir);
                let that_add_len = target_node
                    .map(|n| part_add_len(&n.directions, full_dir.inverted()))
                    .unwrap_or_default();

                let len = if dir.is_diagonal() { len * SQRT_2 } else { len };
                let posf = ctx.screen.world_to_screen(pos.convert(|v| v as f32 + 0.5));

                if self
                    .selection
                    .contains(&SelectedBoardItem::WirePart { pos: part_pos, dir })
                {
                    let wire_width = WIRE_WIDTH + 0.1;

                    let pos1 = posf
                        + Vec2f::from_angle_length(
                            angle + PI,
                            this_add_len * wire_width * ctx.screen.scale,
                        );

                    let pos2 = posf
                        + Vec2f::from_angle_length(
                            angle,
                            (len + that_add_len * wire_width) * ctx.screen.scale,
                        );

                    let line_width = (wire_width * ctx.screen.scale).max(3.0);

                    selecion_renderer
                        .border_buffer
                        .add_quad_line(pos1, pos2, line_width, ());
                }

                let pos1 = posf
                    + Vec2f::from_angle_length(
                        angle + PI,
                        this_add_len * WIRE_WIDTH * ctx.screen.scale,
                    );

                let pos2 = posf
                    + Vec2f::from_angle_length(
                        angle,
                        (len + that_add_len * WIRE_WIDTH) * ctx.screen.scale,
                    );

                let line_width = (WIRE_WIDTH * ctx.screen.scale).max(1.0);
                let color = self
                    .wire_colors
                    .entry(wire.id)
                    .or_insert_with(|| wire.color());

                part_buffer.add_quad_line(pos1, pos2, line_width, color.to_normalized_gamma_f32());
            }

            'draw_point: {
                let Some(wire) = &node.wire else {
                    break 'draw_point;
                };
                'draw_check: {
                    if force_draw_points {
                        break 'draw_check;
                    }

                    let mut dirs = 0;

                    for dist in node.directions.values() {
                        if dist.is_some() {
                            dirs += 1;
                            if dirs > 3 {
                                break 'draw_check;
                            }
                        }
                    }

                    if self
                        .selection
                        .contains(&SelectedBoardItem::WirePoint { pos })
                    {
                        for (dir, dist) in node.directions.iter() {
                            let Some(dist) = dist.as_ref() else {
                                continue;
                            };

                            let (part_dir, part_rev) = dir.into_half();
                            let target = if part_rev {
                                dir.into_dir_isize() * dist.get() as isize + pos
                            } else {
                                pos
                            };

                            if !self.selection.contains(&SelectedBoardItem::WirePart {
                                pos: target,
                                dir: part_dir,
                            }) {
                                break 'draw_check;
                            }
                        }
                    }

                    break 'draw_point;
                }

                let mut straight = 0;
                let mut diagonal = 0;

                for (dir, dist) in node.directions.iter() {
                    if dist.is_none() {
                        continue;
                    }

                    if dir.is_diagonal() {
                        diagonal += 1;
                    } else {
                        straight += 1;
                    }
                }

                let all = straight + diagonal;
                let straight = straight >= diagonal;

                let center_pos = ctx.screen.world_to_screen(pos.convert(|v| v as f32 + 0.5));
                let color = self
                    .wire_colors
                    .entry(wire.id)
                    .or_insert_with(|| wire.color());
                let point_size = remap_clamp(
                    all as f32,
                    4.0..=8.0,
                    WIRE_POINT_WIDTH..=BIG_WIRE_POINT_WIDTH,
                ) * ctx.screen.scale;
                if straight {
                    part_buffer.add_centered_rect(
                        center_pos,
                        point_size,
                        color.to_normalized_gamma_f32(),
                    );
                } else {
                    part_buffer.add_centered_rotated_rect(
                        center_pos,
                        point_size,
                        TAU / 8.0,
                        color.to_normalized_gamma_f32(),
                    );
                }

                if self
                    .selection
                    .contains(&SelectedBoardItem::WirePoint { pos })
                {
                    let point_size = point_size + 0.1 * ctx.screen.scale;
                    if straight {
                        selecion_renderer.border_buffer.add_centered_rect(
                            center_pos,
                            point_size,
                            (),
                        );
                    } else {
                        selecion_renderer.border_buffer.add_centered_rotated_rect(
                            center_pos,
                            point_size,
                            TAU / 8.0,
                            (),
                        );
                    }
                }
            }
        }
    }

    fn draw_wires(&self, ctx: &PaintContext) {
        let part_buffer = self.wire_part_buffer.clone();
        let vertexes = self.vertexes.clone();

        ctx.custom_draw(move |ctx| {
            vertexes.draw(
                ctx.painter.gl(),
                ctx.paint_info.screen_size_px,
                part_buffer.lock().deref(),
            );
        });
    }

    fn draw_pins(&self, ctx: &PaintContext) {
        let buffer = self.pin_buffer.clone();
        let vertexes = self.vertexes.clone();

        ctx.custom_draw(move |ctx| {
            vertexes.draw(
                ctx.painter.gl(),
                ctx.paint_info.screen_size_px,
                buffer.lock().deref(),
            );
        });
    }

    fn draw_circuits(&mut self, ctx: &PaintContext) {
        self.circuits_drawn.clear();

        let editor = self.editor.read();
        let mut pin_buffer = self.pin_buffer.lock();
        pin_buffer.clear();

        for (pos, node) in editor
            .circuits
            .iter_area(ctx.tile_bounds_tl, ctx.tile_bounds_size)
        {
            let draw_pins = node.quarters.values().any(|q| q.is_none());
            let mut draw_pin_color = draw_pins
                .then(|| {
                    node.quarters
                        .values()
                        .filter_map(|q| q.as_ref())
                        .filter_map(|q| q.pin.as_ref())
                        .find_map(|p| p.wire.read().as_ref().map(|w| w.color()))
                })
                .flatten();

            for quarter in QuarterPos::ALL {
                let Some(quarter) = node.quarters.get(quarter) else {
                    continue;
                };

                let circuit = &quarter.circuit;

                if draw_pins && draw_pin_color.is_none() {
                    if let Some(pin) = &quarter.pin {
                        draw_pin_color = Some(circuit.pin_color(pin));
                    }
                }

                if self.circuits_drawn.contains(&circuit.id) {
                    continue;
                }

                let selected = self.selection.contains(&SelectedBoardItem::Circuit {
                    id: circuit.id,
                    pos: circuit.info.read().pos,
                });

                let info = circuit.info.read();
                let size = info.size;
                let pos = info.pos;
                drop(info);

                let mut custom_selection = false;
                let selection = selected.then(|| CircuitSelectionRenderingContext {
                    renderer: self.selection_renderer.clone(),
                    custom_selection: &mut custom_selection,
                });

                let circuit_ctx = CircuitRenderingContext::new(ctx, pos, size, selection);
                circuit.imp.read().draw(&circuit_ctx);

                if selected && !custom_selection {
                    let mut renderer = self.selection_renderer.lock();

                    for (pos, node) in editor.circuits.iter_area(pos, size) {
                        for qpos in QuarterPos::ALL {
                            if !node
                                .quarters
                                .get(qpos)
                                .as_ref()
                                .is_some_and(|q| q.circuit.id == circuit.id)
                            {
                                continue;
                            }

                            let pos = pos.convert(|v| v as f32) + qpos.into_quarter_position_f32();
                            let rect = ctx.screen.world_to_screen_rect(Rect::from_min_size(
                                pos.into(),
                                vec2(0.5, 0.5),
                            ));

                            renderer
                                .fill_buffer
                                .add_new_rect(rect.left_top(), rect.size(), ());

                            let border_rect = rect.expand(ctx.screen.scale * 0.1);

                            renderer.border_buffer.add_new_rect(
                                border_rect.left_top(),
                                border_rect.size(),
                                (),
                            );
                        }
                    }
                }

                self.circuits_drawn.insert(circuit.id);
            }

            if let Some(color) = draw_pin_color {
                let center = pos.convert(|v| v as f32 + 0.5);
                pin_buffer.add_centered_rect(
                    ctx.screen.world_to_screen(center),
                    WIRE_WIDTH * ctx.screen.scale,
                    color.to_normalized_gamma_f32(),
                );
            }
        }
    }

    fn draw_selection(&self, ctx: &PaintContext) {
        let renderer = self.selection_renderer.clone();

        ctx.custom_draw(move |ctx| {
            renderer.lock().draw(&ctx);
        });
    }

    fn draw_wire_debug(&self, ctx: &PaintContext) {
        let editor = self.editor.read();
        for (pos, lookaround, node) in editor
            .wires
            .iter_area_with_lookaround(ctx.tile_bounds_tl, ctx.tile_bounds_size)
        {
            let screen_pos = ctx.screen.world_to_screen_tile(pos);

            let wire_points = node.wire.as_ref().map(|w| w.points.read());
            let backend_point = wire_points.as_ref().and_then(|p| p.get(&pos));

            for dir in Direction8::ALL {
                let dist = *node.directions.get(dir);

                let half = dir.into_half();
                let half = half.1.not().then_some(half.0);
                let backend_point_connected = half
                    .zip(backend_point)
                    .map(|(dir, point)| *point.directions.get(dir));

                let text = match dist {
                    Some(d) => format!("{d}"),
                    None if backend_point_connected.is_some_and(|p| p) => "?".into(),
                    _ => continue,
                };

                let text = WidgetText::from(text);
                let galley =
                    text.into_galley(ctx.ui, Some(false), f32::INFINITY, TextStyle::Monospace);

                let align = Vector2::<Align>::from(dir.into_align2().0);
                let align = align.convert(|v| v.to_factor());

                let pos = screen_pos + (Vec2f::from(ctx.screen.scale) - galley.size()) * align;

                let mut rgb = [60u8; 3];

                if let Some(dist) = dist {
                    rgb[0] = 255;
                    let target_node =
                        lookaround.get_relative(dir.into_dir_isize() * dist.get() as isize);
                    let correct_target = target_node.is_some_and(|n| n.wire.is_some());

                    if correct_target {
                        rgb[1] = 255;
                    }
                }
                if !backend_point_connected.is_some_and(|p| !p) {
                    rgb[2] = 255;
                }

                let color = Color32::from_rgb(rgb[0], rgb[1], rgb[2]);

                ctx.painter.rect_filled(
                    Rect::from_min_size(pos.into(), galley.size()).expand(1.0),
                    Rounding::ZERO,
                    Color32::BLACK.gamma_multiply(0.4),
                );

                ctx.painter.add(TextShape {
                    pos: pos.into(),
                    galley,
                    underline: Stroke::NONE,
                    fallback_color: color,
                    override_text_color: None,
                    opacity_factor: 0.9,
                    angle: 0.0,
                });
            }

            if node.wire.is_some() || backend_point.is_some() != node.wire.is_some() {
                let text = match &node.wire {
                    Some(w) => format!("{}", w.id),
                    None => "?".into(),
                };
                let text = WidgetText::from(text);
                let galley =
                    text.into_galley(ctx.ui, Some(false), f32::INFINITY, TextStyle::Monospace);

                let pos = screen_pos + (ctx.screen.scale / 2.0) - (galley.size() / 2.0);

                let mut rgb = [60u8; 3];

                if let Some(wire) = &node.wire {
                    rgb[0] = 255;
                    let correct_wire = editor
                        .board
                        .wires
                        .read()
                        .get(wire.id)
                        .is_some_and(|board_wire| Arc::ptr_eq(wire, board_wire));
                    if correct_wire {
                        rgb[1] = 255;
                    }
                }
                if backend_point.is_some() {
                    rgb[2] = 255;
                }

                let color = Color32::from_rgb(rgb[0], rgb[1], rgb[2]);

                ctx.painter.rect_filled(
                    Rect::from_min_size(pos.into(), galley.size()).expand(1.0),
                    Rounding::ZERO,
                    Color32::BLACK.gamma_multiply(0.4),
                );

                ctx.painter.add(TextShape {
                    pos: pos.into(),
                    galley,
                    underline: Stroke::NONE,
                    fallback_color: color,
                    override_text_color: None,
                    opacity_factor: 0.9,
                    angle: 0.0,
                });
            }
        }
    }

    fn draw_circuit_debug(&self, ctx: &PaintContext) {
        let editor = self.editor.read();
        for (pos, node) in editor
            .circuits
            .iter_area(ctx.tile_bounds_tl, ctx.tile_bounds_size)
        {
            for qpos in QuarterPos::ALL {
                let Some(quarter) = node.quarters.get(qpos) else {
                    continue;
                };

                let quarter_center = pos.convert(|v| v as f32)
                    + qpos.into_position().convert(|v| v as f32 / 2.0)
                    + 0.25;

                let circuit = &quarter.circuit;

                let text = WidgetText::from(format!("{}", circuit.id));
                let galley =
                    text.into_galley(ctx.ui, Some(false), f32::INFINITY, TextStyle::Monospace);

                let correct_circuit = editor
                    .board
                    .circuits
                    .read()
                    .get(circuit.id)
                    .is_some_and(|ec| Arc::ptr_eq(ec, circuit));

                let info = circuit.info.read();

                let correct_offset_x = quarter.offset.x < info.size.x
                    && quarter.offset.x as isize == (pos.x - info.pos.x);

                let correct_offset_y = quarter.offset.y < info.size.y
                    && quarter.offset.y as isize == (pos.y - info.pos.y);

                drop(info);

                let mut rgb = [60u8; 3];

                if correct_offset_x {
                    rgb[0] = 255;
                }

                if correct_offset_y {
                    rgb[1] = 255;
                }

                if correct_circuit {
                    rgb[2] = 255;
                }

                let color = Color32::from_rgb(rgb[0], rgb[1], rgb[2]);

                let text_pos = ctx.screen.world_to_screen(quarter_center) - galley.size() / 2.0;

                ctx.painter.rect_filled(
                    Rect::from_min_size(text_pos.into(), galley.size()).expand(1.0),
                    Rounding::ZERO,
                    Color32::BLACK.gamma_multiply(0.4),
                );

                ctx.painter.add(TextShape {
                    pos: text_pos.into(),
                    galley,
                    underline: Stroke::NONE,
                    fallback_color: color,
                    override_text_color: None,
                    opacity_factor: 0.9,
                    angle: 0.0,
                });

                if let Some(pin) = &quarter.pin {
                    let wire = pin.wire.read().clone();

                    let text = match &wire {
                        Some(w) => WidgetText::from(format!("{}", w.id)),
                        None => WidgetText::from("X"),
                    };
                    let galley =
                        text.into_galley(ctx.ui, Some(false), f32::INFINITY, TextStyle::Monospace);

                    let wire_point = editor.wires.get(pos).and_then(|n| n.wire.clone());

                    let properly_connected_wire_point = (wire.is_none() && wire_point.is_none())
                        || wire.as_ref().is_some_and(|w| {
                            wire_point.as_ref().is_some_and(|wp| Arc::ptr_eq(w, wp))
                        });

                    let properly_connected_backend = wire.is_none()
                        || wire.is_some_and(|w| {
                            w.connected_pins.read().iter().any(|(c, p)| {
                                Arc::ptr_eq(c, &quarter.circuit) && Arc::ptr_eq(p, pin)
                            })
                        });

                    let correct_quarter_pin = circuit
                        .pins
                        .read()
                        .get(pin.id)
                        .is_some_and(|bp| Arc::ptr_eq(&bp.pin, pin));

                    let mut rgb = [60u8; 3];

                    if properly_connected_wire_point {
                        rgb[0] = 255;
                    }

                    if properly_connected_backend {
                        rgb[1] = 255;
                    }

                    if correct_quarter_pin {
                        rgb[2] = 255;
                    }

                    let color = Color32::from_rgb(rgb[0], rgb[1], rgb[2]);

                    let off = (qpos.into_quarter_position_f32() - 0.5) * 2.0 * galley.size();

                    let pos = ctx.screen.world_to_screen(pos.convert(|v| v as f32 + 0.5)) + off;

                    ctx.painter.rect_filled(
                        Rect::from_min_size(pos.into(), galley.size()).expand(1.0),
                        Rounding::ZERO,
                        Color32::BLACK.gamma_multiply(0.4),
                    );

                    ctx.painter.add(TextShape {
                        pos: pos.into(),
                        galley,
                        underline: Stroke::NONE,
                        fallback_color: color,
                        override_text_color: None,
                        opacity_factor: 0.9,
                        angle: 0.0,
                    });
                }
            }
        }
    }

    fn handle_wire_interactions(
        &mut self,
        ctx: &PaintContext,
        interaction: &Response,
        active: bool,
    ) {
        let world_mouse = ctx
            .ui
            .input(|input| input.pointer.latest_pos())
            .map(|p| ctx.screen.screen_to_world(p));

        let world_mouse_tile = world_mouse.map(|v| v.convert(|v| v.floor() as isize));

        if active
            && interaction.hovered()
            && ctx
                .ui
                .input(|input| input.pointer.button_pressed(PointerButton::Primary))
        {
            self.wire_draw_start = world_mouse_tile;
        }

        if self.wire_draw_start.is_some() && !active {
            self.wire_draw_start = None;
        }

        if let Some((start, end)) = self.wire_draw_start.zip(world_mouse_tile) {
            let startf = start.convert(|v| v as f32 + 0.5);
            let endf = end.convert(|v| v as f32 + 0.5);
            let diff = endf - startf;

            // Clockwise from +Y
            let angle = diff.angle_to_xp() + (TAU / 4.0);

            let segment = (angle / (TAU / 8.0)).round();
            let segment_index = ((segment + 8.0) % 8.0) as usize;
            let direction = Direction8::from_index(segment_index);

            let length = diff.length();

            let length = if direction.is_diagonal() {
                length / std::f32::consts::SQRT_2
            } else {
                length
            };
            let length = length.round() as u32;

            let end = start + direction.into_dir_isize() * length as isize;
            let endf = end.convert(|v| v as f32 + 0.5);

            let startf = ctx.screen.world_to_screen(startf);
            let endf = ctx.screen.world_to_screen(endf);

            let place = !ctx.ui.input(|input| input.modifiers.shift);
            let color = if place {
                Color32::from_gray(100)
            } else {
                Color32::from_rgb(255, 100, 100)
            };

            let add_len = if direction.is_diagonal() {
                WIRE_WIDTH / 2.0 * (22.5f32).to_radians().tan()
            } else {
                WIRE_WIDTH / 2.0
            };

            let vec = direction.into_dir_f32() * add_len * ctx.screen.scale;

            let startf = startf - vec;
            let endf = endf + vec;

            ctx.painter.line_segment(
                [startf.into(), endf.into()],
                Stroke::new(WIRE_WIDTH * ctx.screen.scale, color),
            );

            if !ctx
                .ui
                .input(|input| input.pointer.button_down(PointerButton::Primary))
            {
                if let Some(nonzero_len) = NonZeroU32::new(length) {
                    let mut editor = self.editor.write();
                    match place {
                        true => editor.place_wire(start, direction, nonzero_len),
                        false => editor.remove_wire(start, direction, nonzero_len),
                    }
                }
                self.wire_draw_start = None;
            }
        }

        if self.wire_draw_start.is_some()
            && !ctx
                .ui
                .input(|input| input.pointer.button_down(PointerButton::Primary))
        {
            self.wire_draw_start = None;
        }

        if let Some(world_mouse_tile) = world_mouse_tile {
            if active && interaction.clicked_by(PointerButton::Primary) {
                self.editor.write().toggle_wire_point(world_mouse_tile);
            }
        }
    }

    fn handle_circuit_placement(
        &self,
        interaction: &Response,
        ctx: &PaintContext,
        blueprint: &CircuitBlueprint,
    ) {
        let world_mouse = ctx
            .ui
            .input(|input| input.pointer.latest_pos())
            .map(|p| ctx.screen.screen_to_world(p));

        let Some(world_mouse) = world_mouse else {
            return;
        };

        let world_place_pos = world_mouse - blueprint.size.convert(|v| v as f32 / 2.0);
        let world_place_tile = world_place_pos.convert(|v| v.round() as isize);

        let circuit_ctx = CircuitRenderingContext::new(ctx, world_place_tile, blueprint.size, None);
        blueprint.imp.draw(&circuit_ctx);

        let mut iter = blueprint.pins.iter().map(|pin| {
            (
                pin.pos.convert(|v| v as isize) + world_place_tile,
                pin.display_name.deref(),
                pin.dir,
            )
        });

        draw_pin_labels(ctx, true, &mut iter);

        if interaction.clicked() {
            self.editor
                .write()
                .place_circuit(world_place_tile, blueprint)
                .expect("TODO");
        }
    }

    fn handle_keyboard(&mut self, ui: &mut Ui, app: &mut App) {
        if ui.input(|input| input.key_pressed(Key::Delete))
            && self.selection.iter().next().is_some()
        {
            let mut editor = self.editor.write();
            for item in self.selection.iter() {
                match item {
                    SelectedBoardItem::WirePart { pos, dir } => {
                        let dir = Direction8::from(*dir);
                        let dist = editor.wires.get(*pos).and_then(|n| {
                            n.wire.is_some().then(|| *n.directions.get(dir)).flatten()
                        });
                        if let Some(dist) = dist {
                            editor.remove_wire(*pos, dir, dist);
                        }
                    }
                    SelectedBoardItem::WirePoint { pos } => {
                        editor.remove_wire_point_with_parts(*pos);
                    }
                    SelectedBoardItem::Circuit { id, pos } => {
                        let Some(circuit) = editor.board.circuits.read().get(*id).cloned() else {
                            continue;
                        };

                        if circuit.info.read().pos == *pos {
                            editor.remove_circuit(&circuit);
                        }
                    }
                }
            }
            self.selection.clear();
        }

        if ui.input(|input| input.key_pressed(Key::F9)) {
            match (self.wire_debug, self.circuit_debug) {
                (false, false) => self.wire_debug = true,
                (true, false) => {
                    self.wire_debug = false;
                    self.circuit_debug = true;
                }
                (false, true) => self.circuit_debug = false,
                (true, true) => {
                    self.wire_debug = false;
                    self.circuit_debug = false;
                }
            }
        }

        if ui.input(|input| input.key_pressed(Key::F10)) {
            self.debug_not_render_wires = !self.debug_not_render_wires;
        }

        if ui.input(|input| input.key_pressed(Key::Escape)) {
            app.selected_item = None;
        }
    }
}

fn draw_pin_labels(
    ctx: &PaintContext,
    draw_dots: bool,
    pins: &mut dyn Iterator<Item = (Vec2isize, &str, Option<Direction8>)>,
) {
    let font = TextStyle::Body.resolve(ctx.ui.style());
    let margin = 3.0;

    let font = FontId {
        size: font.size * 1.2,
        ..font
    };

    let full_height = font.size + margin * 2.0;
    let min_scale = full_height / 0.8;

    let text_scale = if ctx.screen.scale < min_scale {
        ctx.screen.scale / min_scale
    } else {
        1.0
    };

    let font = FontId {
        size: font.size * text_scale,
        ..font
    };
    let margin = margin * text_scale;
    let distance = 20.0 * text_scale;

    for (pos, name, dir) in pins {
        let pos = ctx.screen.world_to_screen(pos.convert(|v| v as f32 + 0.5));

        if draw_dots {
            ctx.painter.circle_filled(
                pos.into(),
                ctx.screen.scale * WIRE_WIDTH * 0.5,
                Color32::DARK_GREEN,
            );
        }

        let text = WidgetText::from(name);
        let galley = text.into_galley(ctx.ui, Some(false), f32::INFINITY, font.clone());

        let (text_pos, angle) = match dir {
            None => (
                [pos.x - galley.size().x / 2.0, pos.y + distance + margin].into(),
                0.0,
            ),
            Some(dir) => {
                let distant = matches!(
                    dir,
                    Direction8::DownLeft | Direction8::Left | Direction8::UpLeft
                );

                let text_distance = if distant {
                    distance + galley.size().x + margin
                } else {
                    distance + margin
                };

                let angle = dir.into_angle_xp_cw();

                let yoff_angle = if distant {
                    angle + TAU / 4.0
                } else {
                    angle - TAU / 4.0
                };

                let text_yoff_length = galley.size().y / 2.0;

                let text_offset = Vec2f::from_angle_length(angle, text_distance)
                    + Vec2f::from_angle_length(yoff_angle, text_yoff_length);

                let angle = if distant { angle + TAU / 2.0 } else { angle };

                (pos + text_offset, angle)
            }
        };

        let rect_pos_off_angle = angle - (TAU * 3.0 / 8.0);
        let rect_pos_off_len = (margin * margin * 2.0).sqrt();
        let rect_pos_off = Vec2f::from_angle_length(rect_pos_off_angle, rect_pos_off_len);

        let rect_pos = text_pos + rect_pos_off;
        let rect_size = Vec2f::from(galley.size()) + margin * 2.0;

        let rect = Rect::from_min_size(rect_pos.into(), rect_size.into());

        let rounding = Rounding::same(3.0 * text_scale);
        let fill = Color32::from_gray(30);
        let stroke = Stroke::new(1.0, Color32::from_gray(100));

        let path = rotated_rect(rect, rect_pos, angle, rounding, fill, stroke);
        ctx.painter.add(path);

        ctx.painter.add(TextShape {
            pos: text_pos.floor().into(),
            galley,
            underline: Stroke::NONE,
            fallback_color: ctx.ui.style().visuals.text_color(),
            override_text_color: None,
            opacity_factor: 1.0,
            angle,
        });
    }
}

#[derive(Debug, Clone, Copy)]
pub struct PanAndZoom {
    center_pos: Vec2f,
    scale: f32,
}

impl PanAndZoom {
    fn update(&mut self, ui: &Ui, rect: Rect, drag: bool, interaction: &Response) {
        let zoom = ui.input(|input| {
            input
                .multi_touch()
                .map(|mt| mt.zoom_delta)
                .unwrap_or_else(|| {
                    if interaction.hovered() {
                        let v = input.smooth_scroll_delta.y / 240.0;
                        if v < 0.0 {
                            1.0 / (-v + 1.0)
                        } else if v > 0.0 {
                            v + 1.0
                        } else {
                            1.0
                        }
                    } else {
                        1.0
                    }
                })
        });

        if drag {
            self.center_pos -= interaction.drag_delta() / self.scale;
        }

        if zoom != 1.0 {
            let pointer_screen = Vec2f::from(
                ui.input(|i| i.pointer.hover_pos())
                    .unwrap_or_else(|| rect.center())
                    - rect.left_top(),
            );

            let pointer_center = pointer_screen - (rect.size() / 2.0);

            let world_before = self.center_pos + pointer_center / self.scale;
            self.scale *= zoom;
            let world_after = self.center_pos + pointer_center / self.scale;
            self.center_pos -= world_after - world_before;
        }
    }
}

impl Default for PanAndZoom {
    fn default() -> Self {
        Self {
            scale: 16.0,
            center_pos: Default::default(),
        }
    }
}

impl PanAndZoom {
    pub fn new(center_pos: Vec2f, scale: f32) -> Self {
        Self { center_pos, scale }
    }

    pub fn to_screen(self, screen_rect: Rect) -> Screen {
        let tl_pos = self.center_pos - (screen_rect.size() / 2.0 / self.scale);
        Screen::new(screen_rect, tl_pos, self.scale)
    }
}
