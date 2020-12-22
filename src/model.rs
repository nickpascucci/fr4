use graphics::{math, types};
/// Circuit board model types and functions.
use opengl_graphics::GlGraphics;
use piston::input::RenderArgs;

const BLACK: [f32; 4] = [0.0, 0.0, 0.0, 1.0];
const GREEN: [f32; 4] = [0.0, 1.0, 0.0, 1.0];
const RED: [f32; 4] = [1.0, 0.0, 0.0, 1.0];

pub struct Point {
    x: f64,
    y: f64,
}

// TODO Units of measure support.
pub enum Shape {
    Rectangle { xy1: Point, xy2: Point, rot: u64 },
    // Circle { c: Point, r: u64 },
    // Arc { c: Point, r: u64, a: u64 },
}

impl Shape {
    pub fn render(&self, color: types::Color, transform: math::Matrix2d, gl: &mut GlGraphics) {
        use graphics::*;
        match self {
            Shape::Rectangle { xy1, xy2, rot } => {
                let rect = rectangle::rectangle_by_corners(xy1.x, xy1.y, xy2.x, xy2.y);
                rectangle(color, rect, transform, gl);
            }
        }
    }
}

pub enum Component {
    Pad(Point, Shape),
    Group(Vec<Component>), // Do groups need transformation data to draw?
}

impl Component {
    pub fn render(&self, gl: &mut GlGraphics, args: &RenderArgs) {
        use graphics::*;

        gl.draw(args.viewport(), |c, gl| {
            use Component::*;
            match self {
                Pad(p, s) => {
                    let transform = c.transform.trans(p.x, p.y);
                    s.render(RED, transform, gl);
                }
                Group(cs) => {
                    for c in cs {
                        c.render(gl, args);
                    }
                }
            }
        });
    }
}
