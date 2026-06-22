use std::{
    collections::VecDeque,
    io::{Read, Write},
    ops::Deref,
    sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

use eframe::egui::{Color32, Stroke, StrokeKind};
use parking_lot::{Condvar, Mutex};

use crate::{
    Direction8,
    components::{
        Component, ComponentCtx, ComponentImpl, ComponentPin, ComponentRenderingContext,
        ComponentTransform, ComponentUpdateReason, PinDescription, RealizedPin,
    },
    state::{sim::UpdateTaskPool, wires::WireState},
    str::ArcStaticStr,
    vector::Vec2usize,
};

#[derive(Default)]
pub struct WorldIOState {
    pipes: Option<(PipeTx, PipeRx)>,

    old_recv: bool,
    old_send: bool,
}

struct WorldIOPins {
    tx_ready: Arc<ComponentPin>,
    tx_send: Arc<ComponentPin>,
    tx: [Arc<ComponentPin>; 8],

    rx_ready: Arc<ComponentPin>,
    rx_recv: Arc<ComponentPin>,
    rx: [Arc<ComponentPin>; 8],

    connect: Arc<ComponentPin>,
    connected: Arc<ComponentPin>,
    error: Arc<ComponentPin>,
}

pub struct WorldIOInstance {
    pins: WorldIOPins,
}

#[derive(Clone, Default)]
pub struct WorldIO {}

impl WorldIOPins {
    pub fn create(pins: &[RealizedPin]) -> Self {
        Self {
            tx_ready: pins[0].pin.clone(),
            tx_send: pins[1].pin.clone(),
            tx: std::array::from_fn(|i| pins[2 + i].pin.clone()),

            rx_ready: pins[10].pin.clone(),
            rx_recv: pins[11].pin.clone(),
            rx: std::array::from_fn(|i| pins[12 + i].pin.clone()),

            connect: pins[20].pin.clone(),
            connected: pins[21].pin.clone(),
            error: pins[22].pin.clone(),
        }
    }
}

impl ComponentImpl for WorldIO {
    type State = WorldIOState;

    type Instance = WorldIOInstance;

    fn id(&self) -> ArcStaticStr {
        "worldio".into()
    }

    fn display_name(&self) -> ArcStaticStr {
        "World IO".into()
    }

    fn size(&self, _transform: ComponentTransform) -> Vec2usize {
        (12, 12).into()
    }

    fn describe_pins(&self, _transform: ComponentTransform) -> Box<[PinDescription]> {
        Box::new(std::array::from_fn::<_, 23, _>(|i| match i {
            0 => PinDescription {
                pos: (0, 1).into(),
                id: "txready".into(),
                display_name: "TX Ready".into(),
                dir: Some(Direction8::Left),
                ty: super::PinType::Outside,
            },

            1 => PinDescription {
                pos: (0, 2).into(),
                id: "txsend".into(),
                display_name: "TX Send".into(),
                dir: Some(Direction8::Left),
                ty: super::PinType::Inside,
            },

            2..=9 => {
                let id = i - 2;
                PinDescription {
                    pos: (0, id + 3).into(),
                    id: format!("tx{id}").into(),
                    display_name: format!("TX {id}").into(),
                    dir: Some(Direction8::Left),
                    ty: super::PinType::Inside,
                }
            }

            10 => PinDescription {
                pos: (11, 1).into(),
                id: "rxready".into(),
                display_name: "RX Ready".into(),
                dir: Some(Direction8::Right),
                ty: super::PinType::Outside,
            },

            11 => PinDescription {
                pos: (11, 2).into(),
                id: "rxrecv".into(),
                display_name: "RX Receive".into(),
                dir: Some(Direction8::Right),
                ty: super::PinType::Inside,
            },

            12..=19 => {
                let id = i - 12;
                PinDescription {
                    pos: (11, id + 3).into(),
                    id: format!("rx{id}").into(),
                    display_name: format!("RX {id}").into(),
                    dir: Some(Direction8::Right),
                    ty: super::PinType::Outside,
                }
            }

            20 => PinDescription {
                pos: (1, 0).into(),
                id: "connect".into(),
                display_name: "Connect".into(),
                dir: Some(Direction8::Up),
                ty: super::PinType::Inside,
            },

            21 => PinDescription {
                pos: (2, 0).into(),
                id: "connected".into(),
                display_name: "Connected".into(),
                dir: Some(Direction8::Up),
                ty: super::PinType::Outside,
            },

            22 => PinDescription {
                pos: (3, 0).into(),
                id: "error".into(),
                display_name: "Error".into(),
                dir: Some(Direction8::Up),
                ty: super::PinType::Outside,
            },

            _ => unreachable!(),
        }))
    }

    fn draw(&self, _component: Option<ComponentCtx<Self>>, render: &ComponentRenderingContext) {
        render.paint.rect(
            render.screen_rect.expand(render.paint.screen.scale * -0.5),
            render.paint.screen.scale * 0.25,
            Color32::from_gray(64),
            Stroke::new(0.05 * render.paint.screen.scale, Color32::from_gray(92)),
            StrokeKind::Middle,
        );
    }

    fn create_instance(&self, component: &Arc<Component>) -> Self::Instance {
        WorldIOInstance {
            pins: WorldIOPins::create(component.pins.read().deref()),
        }
    }

    fn pins_changed(&self, component: &Component, instance: &mut Self::Instance) {
        instance.pins = WorldIOPins::create(component.pins.read().deref());
    }

    fn update(&self, mut ctx: ComponentCtx<Self>, reason: ComponentUpdateReason) {
        if let ComponentUpdateReason::StateReset | ComponentUpdateReason::ComponentPlaced = reason {
            ctx.set_pin_output(&ctx.instance.pins.tx_ready, WireState::Bool(false));
            ctx.set_pin_output(&ctx.instance.pins.rx_ready, WireState::Bool(false));
            ctx.set_pin_output(&ctx.instance.pins.connected, WireState::Bool(false));
            ctx.set_pin_output(&ctx.instance.pins.error, WireState::Bool(false));
        }

        if let ComponentUpdateReason::ChangedPin(20) = reason {
            let connected = ctx
                .read_internal_state()
                .map(|s| s.pipes.is_some())
                .unwrap_or(false);
            let connected_pin = matches!(
                ctx.get_pin_input(&ctx.instance.pins.connect),
                WireState::Bool(true)
            );

            if connected != connected_pin {
                ctx.set_pin_output(&ctx.instance.pins.tx_ready, WireState::Bool(false));
                ctx.set_pin_output(&ctx.instance.pins.rx_ready, WireState::Bool(false));

                if connected_pin {
                    let (tx_local, tx_remote) = create_pipe(64);
                    let (rx_remote, rx_local) = create_pipe(64);

                    let tx_ready_pin = ctx.instance.pins.tx_ready.clone();
                    let rx_ready_pin = ctx.instance.pins.rx_ready.clone();

                    let state = ctx
                        .state
                        .board()
                        .simulation()
                        .states()
                        .read()
                        .get(&ctx.state.uid())
                        .unwrap()
                        .clone();
                    let state_clone = state.clone();

                    std::thread::spawn(move || {
                        let mut stdin = std::io::stdin();

                        let mut buf = [0u8; 64];
                        let mut tasks = UpdateTaskPool::default();

                        loop {
                            let read = stdin.read(&mut buf).unwrap();
                            let mut buf = &buf[..read];

                            while !buf.is_empty() {
                                match rx_remote.write(buf) {
                                    Ok(w) => {
                                        buf = &buf[w..];

                                        let mut state = state_clone.state().write();

                                        rx_ready_pin.set_output(
                                            &mut state.components,
                                            &mut tasks,
                                            WireState::Bool(true),
                                        );

                                        state.add_tasks(&mut tasks.drain());

                                        rx_remote.block_until_ready().ok();
                                    }
                                    Err(PipeBroken) => return,
                                }
                            }
                        }
                    });

                    std::thread::spawn(move || {
                        let mut stdout = std::io::stdout();

                        let mut buf = [0u8; 64];
                        let mut tasks = UpdateTaskPool::default();

                        loop {
                            let Ok(read) = tx_remote.read(&mut buf) else {
                                return;
                            };

                            let mut buf = &buf[..read];

                            while !buf.is_empty() {
                                let w = stdout.write(buf).unwrap();
                                stdout.flush().ok();

                                buf = &buf[w..];

                                let mut state = state.state().write();

                                tx_ready_pin.set_output(
                                    &mut state.components,
                                    &mut tasks,
                                    WireState::Bool(true),
                                );

                                state.add_tasks(&mut tasks.drain());
                            }

                            tx_remote.block_until_ready().ok();
                        }
                    });

                    ctx.set_pin_output(&ctx.instance.pins.connected, WireState::Bool(true));
                    ctx.set_pin_output(&ctx.instance.pins.tx_ready, WireState::Bool(true));
                    ctx.write_internal_state().pipes = Some((tx_local, rx_local));
                } else {
                    ctx.set_pin_output(&ctx.instance.pins.connected, WireState::Bool(false));
                    ctx.set_pin_output(&ctx.instance.pins.error, WireState::Bool(false));
                    ctx.set_pin_output(&ctx.instance.pins.tx_ready, WireState::Bool(false));
                    ctx.set_pin_output(&ctx.instance.pins.rx_ready, WireState::Bool(false));
                    ctx.write_internal_state().pipes = None;
                }
            }
        }

        if let ComponentUpdateReason::ChangedPin(1) = reason {
            let send = ctx
                .read_internal_state()
                .map(|s| s.old_send)
                .unwrap_or(false);
            let send_pin = matches!(
                ctx.get_pin_input(&ctx.instance.pins.tx_send),
                WireState::Bool(true)
            );

            if !send
                && send_pin
                && let Some((tx, _)) = ctx.read_internal_state().and_then(|s| s.pipes.as_ref())
            {
                let mut byte = 0;
                for i in 0..=7 {
                    let bit = matches!(
                        ctx.get_pin_input(&ctx.instance.pins.tx[i]),
                        WireState::Bool(true)
                    );
                    if bit {
                        byte |= 1 << i;
                    }
                }

                let free = tx.write_size();
                if free > 0 {
                    if free == 1 {
                        ctx.set_pin_output(&ctx.instance.pins.tx_ready, WireState::Bool(false));

                        let tx = &ctx
                            .read_internal_state()
                            .and_then(|s| s.pipes.as_ref())
                            .unwrap()
                            .0;
                        tx.write(&[byte]).ok();
                    } else {
                        tx.write(&[byte]).ok();
                    }
                }
            }

            ctx.write_internal_state().old_send = send_pin;
        }

        if let ComponentUpdateReason::ChangedPin(11) = reason {
            let recv = ctx
                .read_internal_state()
                .map(|s| s.old_recv)
                .unwrap_or(false);
            let recv_pin = matches!(
                ctx.get_pin_input(&ctx.instance.pins.rx_recv),
                WireState::Bool(true)
            );

            if !recv
                && recv_pin
                && let Some((_, rx)) = ctx.read_internal_state().and_then(|s| s.pipes.as_ref())
            {
                let mut byte = 0;
                let ok = match rx.read_size() {
                    0 => false,
                    1 => {
                        ctx.set_pin_output(&ctx.instance.pins.rx_ready, WireState::Bool(false));

                        let rx = &ctx
                            .read_internal_state()
                            .and_then(|s| s.pipes.as_ref())
                            .unwrap()
                            .1;
                        rx.read(std::slice::from_mut(&mut byte)).ok();

                        true
                    }
                    _ => {
                        rx.read(std::slice::from_mut(&mut byte)).ok();
                        true
                    }
                };

                for i in 0..=7 {
                    let value = if !ok {
                        WireState::None
                    } else {
                        WireState::Bool(byte & (1 << i) != 0)
                    };

                    ctx.set_pin_output(&ctx.instance.pins.rx[i], value);
                }
            }

            ctx.write_internal_state().old_recv = recv_pin;
        }
    }
}

fn create_pipe(capacity: usize) -> (PipeTx, PipeRx) {
    let pipe = Arc::new(Pipe {
        buffer: Mutex::new(VecDeque::with_capacity(capacity)),
        write_ready: Condvar::new(),
        read_ready: Condvar::new(),
        broken: AtomicBool::new(false),
    });

    (PipeTx { pipe: pipe.clone() }, PipeRx { pipe })
}

struct PipeBroken;

struct Pipe {
    buffer: Mutex<VecDeque<u8>>,

    write_ready: Condvar,
    read_ready: Condvar,
    broken: AtomicBool,
}

struct PipeRx {
    pipe: Arc<Pipe>,
}

struct PipeTx {
    pipe: Arc<Pipe>,
}

#[allow(unused)]
impl PipeTx {
    fn is_connected(&self) -> bool {
        self.pipe.broken.load(Ordering::Relaxed)
    }

    fn write_size(&self) -> usize {
        let buffer = self.pipe.buffer.lock();
        buffer.capacity() - buffer.len()
    }

    fn write(&self, data: &[u8]) -> Result<usize, PipeBroken> {
        let mut buffer = self.pipe.buffer.lock();

        if self.pipe.broken.load(Ordering::Relaxed) {
            return Err(PipeBroken);
        }

        let free = buffer.capacity() - buffer.len();
        let write = free.min(data.len());

        buffer.write_all(&data[..write]).unwrap();

        if !buffer.is_empty() {
            self.pipe.read_ready.notify_one();
        }

        Ok(write)
    }

    fn block_until_ready(&self) -> Result<(), PipeBroken> {
        let mut buf = self.pipe.buffer.lock();

        if self.pipe.broken.load(Ordering::Relaxed) {
            return Err(PipeBroken);
        }

        while buf.len() >= buf.capacity() {
            self.pipe.write_ready.wait(&mut buf);

            if self.pipe.broken.load(Ordering::Relaxed) {
                return Err(PipeBroken);
            }
        }

        Ok(())
    }
}

#[allow(unused)]
impl PipeRx {
    fn is_connected(&self) -> bool {
        self.pipe.broken.load(Ordering::Relaxed)
    }

    fn read_size(&self) -> usize {
        let buffer = self.pipe.buffer.lock();
        buffer.len()
    }

    fn read(&self, buf: &mut [u8]) -> Result<usize, PipeBroken> {
        let mut buffer = self.pipe.buffer.lock();

        if self.pipe.broken.load(Ordering::Relaxed) {
            return Err(PipeBroken);
        }

        let read = buffer.len().min(buf.len());

        buffer.read_exact(&mut buf[..read]).unwrap();

        if buffer.len() < buffer.capacity() {
            self.pipe.write_ready.notify_one();
        }

        Ok(read)
    }

    fn block_until_ready(&self) -> Result<(), PipeBroken> {
        let mut buf = self.pipe.buffer.lock();

        if self.pipe.broken.load(Ordering::Relaxed) {
            return Err(PipeBroken);
        }

        while buf.len() >= buf.capacity() {
            self.pipe.read_ready.wait(&mut buf);

            if self.pipe.broken.load(Ordering::Relaxed) {
                return Err(PipeBroken);
            }
        }

        Ok(())
    }
}

impl Drop for PipeTx {
    fn drop(&mut self) {
        let buf = self.pipe.buffer.lock();
        self.pipe.broken.store(true, Ordering::Relaxed);
        drop(buf);

        self.pipe.read_ready.notify_all();
        self.pipe.write_ready.notify_all();
    }
}

impl Drop for PipeRx {
    fn drop(&mut self) {
        let buf = self.pipe.buffer.lock();
        self.pipe.broken.store(true, Ordering::Relaxed);
        drop(buf);

        self.pipe.read_ready.notify_all();
        self.pipe.write_ready.notify_all();
    }
}
