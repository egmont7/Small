#version 330 core

uniform sampler2D m_ColorMap;
uniform sampler2D m_StrengthMap;

uniform vec3 m_Ambient;
uniform vec3 m_LightColor;
uniform vec3 m_LightPosition;
uniform float m_Shininess;
uniform float m_Strength;

uniform vec3  g_CameraPosition;
uniform float m_ConstantAttenuation;
uniform float m_LinearAttenuation;
uniform float m_QuadraticAttenuation;

in vec3 Normal;
in vec3 Position;
in vec3 LightPosition;

in vec2 texCoord1;
in vec2 texCoord2;

out vec4 FragColor;

void main(){
    vec3 color = vec3(1.0);
    vec4 absColor = texture(m_ColorMap, texCoord1);
    color *= absColor.xyz;
    color *= texture(m_StrengthMap, texCoord2).r;
    
    vec3 lightDirection = LightPosition - vec3(Position);
    float lightDistance = length(lightDirection);
    
    lightDirection = lightDirection / lightDistance;

    float attenuation = 1.0 / 
                           (m_ConstantAttenuation + 
                            m_LinearAttenuation * lightDistance +
                            m_QuadraticAttenuation * lightDistance * lightDistance);

    float diffuse = max(0.0, dot(Normal, lightDirection));



    vec3 scatteredLight = m_Ambient + m_LightColor * diffuse * attenuation;
    vec3 rgb = min(color * scatteredLight, vec3(1.0));
    FragColor = vec4(rgb, absColor.a);
    
}