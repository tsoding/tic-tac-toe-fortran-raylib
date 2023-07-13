#version 330

// Input vertex attributes (from vertex shader)
in vec2 fragTexCoord;
in vec4 fragColor;

// Input uniform values
uniform sampler2D texture0;
uniform vec4 colDiffuse;

// Output fragment color
out vec4 finalColor;

void main() {
    vec4 color = texture(texture0, vec2(fragTexCoord.x, 1 - fragTexCoord.y));
    finalColor = color;
}
