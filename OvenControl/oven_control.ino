/** oven_control.ino
 * This code controls a toaster oven to maintain a specific temperature.
 * The setup consists of 
 *   -Generic Toaster oven with infared heater
 *   -Precon HS-2000V Temp/Humid sensor
 *   -A solid state relay(Line voltage out, 5V control)
 *   -Arduino Nano
 * During operation the oven is set to it's highest temperature and timer settings so the
 * element is on or off depending only on whether the oven is being supplied power, which
 * in turn depends on whether the relay is switched on or off by a control signal supplied 
 * by the Arduino. 
 *
 * Using this script:
 * To adapt this code to your use, do the following
 *   -Adjust TEMP_LINE and OVEN_CONTROL_LINE to the ports leading to your temperature sensor,
 *     and relay, respectively
 *   -Set your desired temperature and variation by adjusting TEMP_TARGET, and TEMP_VARIATION
 *   -Estimate(or measure) the heating speed of your setup and place that value in HEAT_SPEED
 *   -Depending on the placement of the temperature sensor, MEASURE_DELAY may need to be adjusted
 *   -An companion Python script may be run on the host computer to record a temperature log.
 */

int TEMP_LINE = A0;
int OVEN_CONTROL_LINE = 2; //Digital line number
int DELAY_MS = 1000;


float TEMP_TARGET = 50;  // degrees C
float TEMP_VARIATION = 3;// degrees C
                         //smaller variation --> More constant temperature
                         // larger variation  --> less frequent switching
float HEAT_SPEED = 0.8; // heating speed of oven (degrees C / second)

int MEASURE_DELAY = 20; //Time between heating and further action units of DELAY_MS

float T_HIGH = TEMP_TARGET + TEMP_VARIATION;
float T_LOW  = TEMP_TARGET - TEMP_VARIATION;


/** Returns time to heat oven to T_HIGH in units of DELAY_MS
 */
int get_heat_time(float t_current){
  float time_f = (T_HIGH - t_current) / HEAT_SPEED;
  return ceil(time_f * 1000. / DELAY_MS);
}

float measure_temp(){
  int adc_value = analogRead(TEMP_LINE);
  return 130*(adc_value / 1024.0) - 30;
}


boolean oven_on = false;
void enable_oven(){
   oven_on = true;
   Serial.println("#-->Oven ON");
   digitalWrite(OVEN_CONTROL_LINE, 1); 
}

void disable_oven(){
  oven_on = false;
  Serial.println("#-->Oven OFF");
  digitalWrite(OVEN_CONTROL_LINE, 0); 
}

float to_seconds(int delay_units){
  return float(delay_units) * 1000. / DELAY_MS;
}

void setup() {
  Serial.begin(9600);
  analogReference(EXTERNAL);
  pinMode(OVEN_CONTROL_LINE, OUTPUT);
}

int action_hold = -1;
int heat_timer = -1;
float cycle_max = 0;
void loop() {
  float t = measure_temp();
  if(t > cycle_max) cycle_max = t;
  Serial.println(t);
  
  if(heat_timer > 0){
    heat_timer--;
  }
  else if (heat_timer == 0){
    disable_oven();
    heat_timer = -1;
  }
  
  if(action_hold == -1){
    if(t < T_LOW){
      heat_timer = get_heat_time(t);
      Serial.print("#-->Enabling Oven for ");
      Serial.print(to_seconds(heat_timer));
      Serial.print(" seconds\n");
      
      Serial.print("#-->Previous Heat Cycle missed target by ");
      Serial.print(cycle_max - T_HIGH);
      Serial.print(" degrees C\n");
      cycle_max = 0;
      action_hold = heat_timer + MEASURE_DELAY;
      enable_oven();
    }
  } else if(action_hold == 0){
    action_hold = -1;
  } else {
    action_hold--;
  }
  delay(DELAY_MS);
}