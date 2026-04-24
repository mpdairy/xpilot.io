use std::cell::Cell;
use std::rc::Rc;

use shared::protocol::TickInput;
use wasm_bindgen::prelude::*;
use wasm_bindgen::JsCast;
use web_sys::{KeyboardEvent, Window};

/// Live keyboard state. The game loop snapshots this into a `TickInput` each
/// fixed tick. Cells, not RefCell, because all writes are short and
/// non-overlapping with reads.
#[derive(Default)]
pub struct InputState {
    pub turn_left: Cell<bool>,
    pub turn_right: Cell<bool>,
    pub thrust: Cell<bool>,
    pub fire: Cell<bool>,
}

impl InputState {
    pub fn snapshot(&self, client_tick: u32) -> TickInput {
        TickInput {
            client_tick,
            turn_left: self.turn_left.get(),
            turn_right: self.turn_right.get(),
            thrust: self.thrust.get(),
            fire: self.fire.get(),
        }
    }

    /// Drop every held key. Called on blur / page hide so a key held while the
    /// tab loses focus doesn't get stuck — the browser swallows the keyup.
    pub fn clear(&self) {
        self.turn_left.set(false);
        self.turn_right.set(false);
        self.thrust.set(false);
        self.fire.set(false);
    }

    fn apply(&self, code: &str, down: bool) -> bool {
        match code {
            "KeyA" | "ArrowLeft" => self.turn_left.set(down),
            "KeyS" | "ArrowRight" => self.turn_right.set(down),
            "ShiftLeft" | "ShiftRight" | "ArrowUp" => self.thrust.set(down),
            "Enter" | "Space" | "ControlLeft" | "ControlRight" | "KeyX" => self.fire.set(down),
            _ => return false,
        }
        true
    }
}

/// Wire window keydown/keyup to the shared input state. Stores the closures in
/// the window so they outlive this call (we never tear them down — the page is
/// the lifecycle).
pub fn install_listeners(window: &Window, state: Rc<InputState>) -> Result<(), JsValue> {
    let s = state.clone();
    let down = Closure::<dyn FnMut(KeyboardEvent)>::new(move |ev: KeyboardEvent| {
        if s.apply(&ev.code(), true) {
            ev.prevent_default();
        }
    });
    window.add_event_listener_with_callback("keydown", down.as_ref().unchecked_ref())?;
    down.forget();

    let s = state;
    let up = Closure::<dyn FnMut(KeyboardEvent)>::new(move |ev: KeyboardEvent| {
        if s.apply(&ev.code(), false) {
            ev.prevent_default();
        }
    });
    window.add_event_listener_with_callback("keyup", up.as_ref().unchecked_ref())?;
    up.forget();

    Ok(())
}
