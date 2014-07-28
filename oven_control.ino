int TEMP_LINE = A0;
int OVEN_CONTROL_LINE = 2;
int DELAY_MS = 1000;

int TEMP_LOW = 45;//Typically ~5 degrees under actual target temp

float get_temp(){
  int adc_value = analogRead(TEMP_LINE);
  return 130*(adc_value / 1024.0) - 30;
}



void setup() {
  Serial.begin(9600);
  analogReference(EXTERNAL);
  pinMode(OVEN_CONTROL_LINE, OUTPUT);
}

void loop() {
  float t = get_temp();
  if(t > TEMP_LOW){
   digitalWrite(OVEN_CONTROL_LINE, 0); 
  } else if( t < TEMP_LOW){
   digitalWrite(OVEN_CONTROL_LINE, 1); 
  }
  Serial.println(t);
  delay(DELAY_MS);
}
