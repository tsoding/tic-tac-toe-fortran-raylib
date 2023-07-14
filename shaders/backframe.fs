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
    float alpha = 1;
    // NOTE: fixing alpha to be always one is important for the backframe because
    // of the particles. They are faded away by slowing animating their alpha
    // towards 0. For some reason it affects the alpha of their fragments in the
    // backframe texture resulting in particles getting darker instead of
    // fading out.
    finalColor = vec4(color.xyz, alpha);
}
