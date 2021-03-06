//! Circuit board model types and functions.

use graphics::{math, types};
use opengl_graphics::GlGraphics;
use piston::input::RenderArgs;

const GREEN: [f32; 4] = [0.0, 1.0, 0.0, 1.0];
const RED: [f32; 4] = [1.0, 0.0, 0.0, 1.0];

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Point {
    pub x: u64,
    pub y: u64,
}
// TODO Nicer Display implementations. One idea is to use Unicode glyphs for shapes, and to not
// print all of their coordinates and details unless asked.

// TODO Units of measure support.
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Shape {
    Rectangle { xy1: Point, xy2: Point },
    // Circle { c: Point, r: u64 },
    // Arc { c: Point, r: u64, a: u64 },
    // TODO Transformed shapes?
}

impl Shape {
    pub fn render(&self, color: types::Color, transform: math::Matrix2d, gl: &mut GlGraphics) {
        use graphics::*;
        match self {
            Shape::Rectangle { xy1, xy2 } => {
                let rect = rectangle::rectangle_by_corners(
                    xy1.x as f64,
                    xy1.y as f64,
                    xy2.x as f64,
                    xy2.y as f64,
                );
                rectangle(color, rect, transform, gl);
            }
        }
    }
}

/// Whether a shape adds or subtracts from a region's shape.
#[derive(Debug, Clone, PartialEq)]
pub enum Polarity {
    /// Remove this shape from the region, leaving a hole.
    Subtract,
    /// Fill in the area of the region defined by this shape.
    Add,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Layered {
    pub layer: usize,
    pub polarity: Polarity,
    pub shape: Shape,
}

impl Layered {
    pub fn render(&self, gl: &mut GlGraphics, args: &RenderArgs) {
        gl.draw(args.viewport(), |c, gl| {
            // TODO Draw each layer separately, then combine
            // TODO Draw layers in different colors
            self.shape.render(GREEN, c.transform, gl);
        });
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Component {
    Board(Layered),
    Pad(Layered),
    Group(Vec<Component>), // Do groups need transformation data to draw?
}

impl Component {
    pub fn render(&self, gl: &mut GlGraphics, args: &RenderArgs) {
        gl.draw(args.viewport(), |c, gl| {
            use Component::*;
            match self {
                Board(l) => {
                    l.render(gl, args);
                }
                Pad(l) => {
                    l.render(gl, args);
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
