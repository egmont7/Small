#version 330 core

uniform mat4 g_WorldViewProjectionMatrix;
uniform mat4 g_WorldViewMatrix;
uniform mat4 g_ViewMatrix;
uniform mat3 g_NormalMatrix;
uniform vec3 m_LightPosition;

in vec4 inPosition;
in vec2 inTexCoord;
in vec2 inTexCoord2;
in vec3 inNormal;

out vec2 texCoord1;
out vec2 texCoord2;

out vec3 Normal;
out vec3 Position;

out vec3 LightPosition;

void main(){
    texCoord1 = inTexCoord;
    texCoord2 = inTexCoord2;
    
    Normal = normalize(g_NormalMatrix * inNormal);
    //Normal = normalize(g_WorldViewMatrix * vec4(inNormal,1.0)).xyz;
    Position = vec3(g_WorldViewMatrix * inPosition);
    LightPosition = vec3(g_ViewMatrix * vec4(m_LightPosition,1.0));

    gl_Position = g_WorldViewProjectionMatrix * inPosition;
}