use opengl_graphics::{GlGraphics, OpenGL};
use piston::event_loop::{EventSettings, Events};
use piston::input::{RenderArgs, RenderEvent, UpdateArgs, UpdateEvent};
use piston::window::WindowSettings;
use sdl2_window::Sdl2Window as Window;
use std::sync::{Arc,RwLock, mpsc::channel};
use std::thread;

mod bdl;
mod model;

pub struct App {
    // The model is shared between the renderer and the Forth interpreter.
    model: Arc<RwLock<model::Component>>,
    gl: GlGraphics, // OpenGL drawing backend.
}

impl App {
    fn render(&mut self, args: &RenderArgs) {
        // Clear the screen.
        graphics::clear(graphics::color::BLACK, &mut self.gl);

        // Render model.
        self.model.read().unwrap().render(&mut self.gl, args);
    }

    fn update(&mut self, _args: &UpdateArgs) {}
}

fn main() {
    // let opengl = OpenGL::V3_2;
    let opengl = OpenGL::V2_1;

    let mut window: Window = WindowSettings::new("FR4", (640, 480))
        .graphics_api(opengl)
        .fullscreen(false)
        .vsync(true)
        .exit_on_esc(true)
        .build()
        .unwrap();

    let mut app = App {
        gl: GlGraphics::new(opengl),
        model: Arc::new(RwLock::new(model::Component::Group(Vec::new()))),
    };

    let bdl_ref = app.model.clone();
    let (tx, rx) = channel();
    thread::spawn(move || {
        let mut ctxt = bdl::Context::new(bdl_ref);
        ctxt.interpret();
        tx.send(true).unwrap();
    });

    let mut events = Events::new(EventSettings::new());
    while let Some(e) = events.next(&mut window) {
        if let Some(args) = e.render_args() {
            app.render(&args);
        }

        if let Some(args) = e.update_args() {
            app.update(&args);
        }

        if let Ok(_) = rx.try_recv() {
            // Forth interpreter has exited, end the program.
            break;
        }
    }
}
