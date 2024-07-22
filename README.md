# How would I use this?
Assuming you know what you're doing (see below if not!), just #load pga.jai and (optionally!) pga_helpers.jai.

pga.jai is just the bare math. It gives you access to points/planes/lines/dual quaternions/flectors as structs. It also defines the important products on them: geometric product, reverse, dual. It also has the wedge product, inner product, and regressive product but only for those situations where, so far as I can tell, it's actually used, eg you only tend to take the regressive product with at least one argument being a point.

## Where can I learn about Projective Geometric Algebra (PGA)?
