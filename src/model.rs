/// Circuit board model types and functions.
use opengl_graphics::GlGraphics;
use piston::input::RenderArgs;

pub struct Point {
    x: u64,
    y: u64,
}

// TODO Units of measure support.
pub enum Shape {
    Rectangle { xy1: Point, xy2: Point, rot: u64 },
    Circle { c: Point, r: u64 },
    Arc { c: Point, r: u64, a: u64 },
}

pub enum Component {
    Pad(Point, Shape),
    Group(Vec<Component>), // Do groups need transformation data to draw?
}

impl Component {
    pub fn render(&self, gl: &mut GlGraphics, args: &RenderArgs) {
        use graphics::*;

        const BLACK: [f32; 4] = [0.0, 0.0, 0.0, 1.0];
        const GREEN: [f32; 4] = [0.0, 1.0, 0.0, 1.0];
        const RED: [f32; 4] = [1.0, 0.0, 0.0, 1.0];

        let square = rectangle::square(0.0, 0.0, 50.0);
        let (x, y) = (args.window_size[0] / 2.0, args.window_size[1] / 2.0);

        gl.draw(args.viewport(), |c, gl| {
            // Clear the screen.
            clear(BLACK, gl);

            let transform = c.transform.trans(x, y).trans(-25.0, -25.0);

            // Draw a box rotating around the middle of the screen.
            rectangle(RED, square, transform, gl);
        });
    }
}
