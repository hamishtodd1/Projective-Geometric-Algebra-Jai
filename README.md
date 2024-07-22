# How would I use this?
Assuming you know what you're doing (see below if not!), just #load pga.jai and (optionally!) pga_helpers.jai.

pga.jai is just the bare math. It gives you access to points/planes/lines/dual quaternions/flectors as structs. It also defines the important products on them: geometric product, reverse, dual. It also has the wedge product, inner product, and regressive product but only for those situations where, so far as I can tell, it's actually used, eg you only tend to take the regressive product with at least one argument being a point.

pga_helpers.jai adds some helper functions whose implementation is a bit more subjective.

## Where can I learn about Projective Geometric Algebra (PGA)?
https://www.youtube.com/watch?v=en2QcehKJd8
https://www.youtube.com/watch?v=dSe7eg8Dj98

I sometimes develop this library live on this twitch channel:
https://www.twitch.tv/hamish_todd

## Who are you?
I'm a former GPU developer (Huawei and Imagination), and I'm now making an animation program for kids. I use this library in that project (which is not open source).

At Imagination I worked on implementing PGA in hardware. There is currently no shader code in this repo; when there is I will see about using the GPU features I worked on adding while I was there there.

## Can I ask you questions about this?
Sure, my twitter is hamish_todd and my email is my first name and surname with a full stop between followed by the numeral 1, at google's email service. You can also hop on a stream and ask a question; I do watch the chat and 8 times out of 10 I will answer a question immediately.