#version 330

// Input vertex attributes (from vertex shader)
in vec2 fragTexCoord;
in vec4 fragColor;

// Input uniform values
uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform float time;

// Output fragment color
out vec4 finalColor;

void main() {
    vec2 uv = fragTexCoord;
    vec2 cPos = 2.0*uv - 1.0;
    float cLength = length(cPos);
    vec2 tex_uv = uv + (cPos/cLength)*mix(cos(cLength*12.0-time*4.0)*0.03, 0.0, cLength / 0.25);
    finalColor = texture(texture0, vec2(tex_uv.x, 1 - tex_uv.y));
}
