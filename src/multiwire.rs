#[derive(Debug, Clone, Copy)]
pub enum MultiwireTargetState {
    CurrentState,
    Uid(u128)
}

#[derive(Debug, Clone, Copy)]
pub struct MultiwireRoute {
    pub target_state: MultiwireTargetState,
    pub wire_id: usize
}

pub trait MultiwireRouter: Send + Sync {
    fn route(&self, pin: usize, routes: &mut Vec<MultiwireRoute>);
}

pub struct DummyRouter;

impl MultiwireRouter for DummyRouter {
    fn route(&self, _pin: usize, _routes: &mut Vec<MultiwireRoute>) {}
}