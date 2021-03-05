use argh::FromArgs;
use opengl_graphics::{GlGraphics, OpenGL};
use piston::event_loop::{EventSettings, Events};
use piston::input::{RenderArgs, RenderEvent, UpdateArgs, UpdateEvent};
use piston::window::WindowSettings;
use sdl2_window::Sdl2Window as Window;
use std::path::PathBuf;
use std::sync::{mpsc::channel, Arc, RwLock};
use std::thread;

mod bdl;
mod model;

#[derive(FromArgs)]
/// Design printed circuit boards using a programming language.
struct Args {
    /// don't show a graphical preview
    #[argh(switch)]
    headless: bool,

    /// script file to execute
    #[argh(positional)]
    script: Option<PathBuf>,
}

pub struct App {
    // The model is shared between the renderer and the Forth interpreter.
    model: Arc<RwLock<model::Component>>,
    gl: Option<GlGraphics>, // OpenGL drawing backend.
}

impl App {
    fn render(&mut self, args: &RenderArgs) {
        match self.gl {
            Some(ref mut g) => {
                // Clear the screen.
                graphics::clear(graphics::color::BLACK, g);

                // Render model.
                self.model.read().unwrap().render(g, args);
            }
            None => (),
        }
    }

    fn update(&mut self, _args: &UpdateArgs) {}
}

fn main() {
    let args: Args = argh::from_env();

    let mut app = App {
        gl: None,
        model: Arc::new(RwLock::new(model::Component::Group(Vec::new()))),
    };

    // Spin up the Forth interpreter thread
    let bdl_ref = app.model.clone();
    let (tx, rx) = channel();
    let script = args.script;

    thread::spawn(move || {
        let mut ctxt = bdl::Context::new(bdl_ref);
        let res = if let Some(path) = script {
            ctxt.interpret_file(&path).and_then(|_| ctxt.interpret())
        } else {
            ctxt.interpret()
        };
        tx.send(res).unwrap();
    });

    let res: Result<(), bdl::Error> = if args.headless {
        loop {
            if let Ok(e) = rx.try_recv() {
                // Forth interpreter has exited, end the program.
                break e;
            }
        }
    } else {
        // TODO Add compiler target macros to set this based on the operating system
        // let opengl = OpenGL::V2_1; // Works on Raspberry Pi
        let opengl = OpenGL::V3_2; // Works on MacOS

        let mut window: Window = Window::new(
            &WindowSettings::new("FR4", (640, 480))
                .graphics_api(opengl)
                .fullscreen(false)
                .vsync(true)
                .exit_on_esc(true),
        )
        .unwrap();

        app.gl = Some(GlGraphics::new(opengl));

        let mut events = Events::new(EventSettings::new());

        loop {
            if let Some(e) = events.next(&mut window) {
                if let Some(args) = e.render_args() {
                    app.render(&args);
                }

                if let Some(args) = e.update_args() {
                    app.update(&args);
                }
            } else {
                break Ok(());
            }

            if let Ok(e) = rx.try_recv() {
                // Forth interpreter has exited, end the program.
                break e;
            }
        }
    };

    if !res.is_ok() {
        // TODO provide more informative error codes
        println!("Error: {:?}", res);
        std::process::exit(1);
    }
}
