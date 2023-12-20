const canvas = document.createElement("canvas")
const gl = canvas.getContext("webgl2")

document.title = "🤖"
document.body.innerHTML = ""
document.body.appendChild(canvas)
document.body.style = "margin:0;touch-action:none;overflow:hidden"
canvas.style.width = "100%"
canvas.style.height = "auto"
canvas.style.userSelect = "none"

const dpr = window.devicePixelRatio

function resize() {
  const {
    innerWidth: width,
    innerHeight: height
  } = window

  canvas.width = width * dpr
  canvas.height = height * dpr

  gl.viewport(0, 0, width * dpr, height * dpr)
}
window.onresize = resize

const vertexSource = `#version 300 es
  #ifdef GL_FRAGMENT_PRECISION_HIGH
  precision highp float;
  #else
  precision mediump float;
  #endif
  
  in vec4 position;
  
  void main(void) {
      gl_Position = position;
  }
  `

const fragmentSource = `#version 300 es
/*********
* made by Matthias Hurrle (@atzedent) 
*/
#ifdef GL_FRAGMENT_PRECISION_HIGH
precision highp float;
#else
precision mediump float;
#endif

// this particular pixel's color
out vec4 fragColor;

uniform vec2 resolution;
uniform vec2 touch;
uniform int pointerCount;
uniform float time;

#define P pointerCount
#define mouse (touch/resolution)

#define T time
#define S smoothstep

mat2 rot(float a) {
	float s=sin(a), c=cos(a);

	// 2D rotation matrix
	return mat2(c,-s,s,c);
}

float pumpkin(vec3 p) {
	// a squished sphere with fancy ripples
	const float r = 1.; // radius of the sphere
	float a = sin(10.*atan(p.x,p.z)); // ripples
	a = 1.-sqrt(abs(a)); // flat out surface of ripples and sharpen ridges
	p *= 1.+a*vec3(.05,.025,.05); // apply ripples
	p.y *= 1.+.5*(r-length(p.xz))/r; //squish sphere

	return length(p)-r; // sdf of a sphere with radius r
}

float mouth(vec3 p) {
	// no mouth at the back
	if (p.z > .0) return 5e5;

	// cylinders
	float a = length(p.xy+vec2(0,-.15))-.5; // upper lip
	float b = length(p.xy+vec2(0,-.55))-.7; // lower

	return max(a, -b); // carve out lower lip
}

float nose(vec3 p) {
	// no nose at the back
	if (p.z > .0) return 5e5;

	// equilateral triangle
	vec2 q = p.xy+vec2(0,-.15); // position
  q.x = abs(q.x); // mirror along y-axis

  float d = dot(q, normalize(vec2(1,.5))); // aspect ratio
	d = max(d, -(q.y+.15)); // size

  return d;
}

float eyes(vec3 p) {
	// no eyes at the back
	if (p.z > .0) return 5e5;

	vec2 q = p.xy;
  q.x = abs(q.x); //mirror
  q -= vec2(.4,.4); // position (doubles the eye, too)

	// sides of a scalene right triangle
  float d = dot(q, normalize(vec2(-.4, .95))); // ratio
  d = max(d, dot(q, normalize(vec2(.95, .4)))); // // ratio
  d = max(d, -.125+dot(q, normalize(vec2(0, -1)))); // size

  return d;
}

float stalk(vec3 p) {
	vec3 q = p;
	q.x += -(q.y-.5)*.1; // inclination

	// cylinder, capped by two planes
	float d = length(q.xz)-.07; // thickness
	d = max(d, p.y-.95 + p.x*.25); // angle cut on top
	d = max(d, -(p.y-.4));

	return d;
}

vec2 map(vec3 p) {
	float
	bdy = pumpkin(p),
	mth = mouth(p),
	nse = nose(p),
	eys = eyes(p),
	slk = stalk(p);

	float d = max(pumpkin(p),-(length(p)-.65)); // carve out inner hollow
	d = max(d, -mth); // carve mouth
	d = max(d, -nse); // carve nose
	d = max(d, -eys); // carve eyes

	vec2 a = vec2(d*.5, 1); // define head (scale sdf down by a half for higher precision ray marching)

	a = a.x < slk ? a : vec2(slk, 2); // whats the nearest sdf, head or stalk?

	return a;
}

vec3 norm(vec3 p) {
	float h=1e-3;
	vec2 k=vec2(-1,1);
	// sample four points of the surface where the ray hit, to calculate the normal
	return normalize(
		k.xyy*map(p+k.xyy*h).x+
		k.yxy*map(p+k.yxy*h).x+
		k.yyx*map(p+k.yyx*h).x+
		k.xxx*map(p+k.xxx*h).x
	);
}

float getshadow(vec3 p, vec3 rd) {
	const float steps = 10., k = 128.;
	float shade = 1.;
	
	for (float i=1e-3; i<steps;) {
		// march the shadows
		float d = map(p + rd * i).x;

    if (d < 1e-3) {
      shade = 5e-3;
      break;
    }

    shade = min(shade, k * d / i);

    i += d;
  }

  return shade;
}

float getocc(vec3 p, vec3 rd) {
	float sca = 2., occ = .0;
	
	for(float i=.0; i<5.; i++) {
		// march ambient occlusion
		float d = 1e-2 + i*.125,
		dd = map(p + rd * d).x;
		occ += (d-dd) * sca;
		sca *= .7;
	}
	
	return clamp(1.-occ, .0, 1.);
}

vec3 shade(vec3 p, vec3 rd, float id) {
	// define color (night) and get the normal of the surface at point p
	vec3 col=vec3(0), n=norm(p),
	// place an outside light source
	lp=vec3(0,1,-3);
	
	float albedo=.2,
	// calculate the light from a bulb in front and above the origin
	diff=clamp(dot(n, normalize(lp)),.0,1.),
	// calculate fresnel
	fres=1.+clamp(dot(-rd,n),.0,1.),
	// distance from light source with a minimum to avoid dividing by zero 
	ldst = max(length(lp-p), 1e-3),
	// light is fading out by this factor
  attn = 1./(1.+ldst*.125+ldst*ldst*.05),
  // these are the shadows cast by the outside light
	shds = getshadow(p+n*5e-2, normalize(lp-p));
	
	fres = pow(fres, 4.); // brighten surfaces and darken crevices even more
	
	col += albedo*fres*diff*.125; // dim outside light
	col *= shds*exp(diff*shds)*fres;
	col *= getocc(p,n)*attn;

	// place flickering light inside
	lp = vec3(sin(T*20.)*.05);
	col += (1.-(length(lp-p)-.1))*vec3(3,2,1);
	ldst = max(length(lp-p), 1e-3),
  attn = 1./(1.+ldst*.125+ldst*ldst*.05),
	col *= getocc(p,n)*attn;
	
	// coloring
	if (id == 1.) { // head
		col *= vec3(.3,.1,.005);
	} else if (id == 2.) { // stalk
		col *= vec3(.125,.2,.005);
	}

	return col;
}

void cam(inout vec3 p) {
	if (P>0) { // user interaction
		// rotate along x at most 180 degrees, centered on the screen
		p.yz *= rot(-mouse.y*3.1415+1.5707);
		// rotate along y full range (360), centered on the screen
		p.xz *= rot(3.1415-mouse.x*6.28322);
	} else {
		// animate camera
		p.yz *= rot(sin(T*.2)*.125);
		p.xz *= rot(-cos(T*.1)*.3);
	}
}

void main(void) {
	// normalize coordinates from -.5 to .5 along both axis
	vec2 uv = (
		gl_FragCoord.xy-.5*resolution
	)/min(resolution.x,resolution.y); // support both orientations

	vec3 col = vec3(0), // in the beginning there was darkness
	p = vec3(0,0,-3), // place the camera 3 units in front of the target
	rd = normalize(vec3(uv,1)); // look with no zoom at the origin

	cam(p); // rotate camera
	cam(rd); // rotate look-at accordingly

	const float steps = 400., maxd = 5.; // settings for raymarcher

	// march along the camera's line of sight towards the target
	for (float i= .0; i<steps; i++) {
		// sample a point
		vec2 d=map(p);

		if (d.x < 1e-3) {
			// here is a surface that wants to be seen
			col = shade(p,rd,d.y);
			break;
		}

		if (d.x > maxd) {
			// left all interesting sights, so lets place a backdrop here,
			// with a nice subtle radial gradient to let jack o'lantern shine
			col = (S(-.5,2.5,-(length(uv)-.25)))*vec3(.005,.005,.1);
			break;
		}

		// move nearer d.x units towards the nearest surface
		p += rd*d.x;
	}

	// apply gamma correction
	col = pow(col, vec3(.4545));
	// bring out the shadows and make it pop
	col = S(.0,1.,col);

	fragColor = vec4(col, 1);
}
  `

function compile(shader, source) {
  gl.shaderSource(shader, source)
  gl.compileShader(shader);

  if (!gl.getShaderParameter(shader, gl.COMPILE_STATUS)) {
    console.error(gl.getShaderInfoLog(shader))
  }
}

let program

function setup() {
  const vs = gl.createShader(gl.VERTEX_SHADER)
  const fs = gl.createShader(gl.FRAGMENT_SHADER)

  compile(vs, vertexSource)
  compile(fs, fragmentSource)

  program = gl.createProgram()

  gl.attachShader(program, vs)
  gl.attachShader(program, fs)
  gl.linkProgram(program)

  if (!gl.getProgramParameter(program, gl.LINK_STATUS)) {
    console.error(gl.getProgramInfoLog(program))
  }
}

let vertices, buffer

function init() {
  vertices = [
      -1., -1., 1.,
      -1., -1., 1.,
      -1., 1., 1.,
      -1., 1., 1.,
    ]

  buffer = gl.createBuffer()

  gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array(vertices), gl.STATIC_DRAW)

  const position = gl.getAttribLocation(program, "position")

  gl.enableVertexAttribArray(position)
  gl.vertexAttribPointer(position, 2, gl.FLOAT, false, 0, 0)

  program.resolution = gl.getUniformLocation(program, "resolution")
  program.time = gl.getUniformLocation(program, "time")
  program.touch = gl.getUniformLocation(program, "touch")
  program.pointerCount = gl.getUniformLocation(program, "pointerCount")
}

const mouse = {
  x: 0,
  y: 0,
  touches: new Set(),
  update: function(x, y, pointerId) {
    this.x = x * dpr;
    this.y = (innerHeight - y) * dpr;
    this.touches.add(pointerId)
  },
  remove: function(pointerId) { this.touches.delete(pointerId) }
}

function loop(now) {
  gl.clearColor(0, 0, 0, 1)
  gl.clear(gl.COLOR_BUFFER_BIT)
  gl.useProgram(program)
  gl.bindBuffer(gl.ARRAY_BUFFER, buffer)
  gl.uniform2f(program.resolution, canvas.width, canvas.height)
  gl.uniform1f(program.time, now * 1e-3)
  gl.uniform2f(program.touch, mouse.x, mouse.y)
  gl.uniform1i(program.pointerCount, mouse.touches.size)
  gl.drawArrays(gl.TRIANGLES, 0, vertices.length * .5)
  requestAnimationFrame(loop)
}

setup()
init()
resize()
loop(0)

window.addEventListener("pointerdown", e => mouse.update(e.clientX, e.clientY, e.pointerId))
window.addEventListener("pointerup", e => mouse.remove(e.pointerId))
window.addEventListener("pointermove", e => {
  if (mouse.touches.has(e.pointerId))
    mouse.update(e.clientX, e.clientY, e.pointerId)
})