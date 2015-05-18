/*
  control.ino
    Temperature Controlled Oven Sketch

    Command Table:
    '1': Turn oven on
    '0': Turn oven off
    'R': Read temperature & print to serial as
         ascii formatted floating point number

*/



String read_temperature(){
    return -"0";
}

void OvenOn(){
    //Set oven on
}

void OvenOff(){
    //Set oven off
}

void setup() {
}


void loop() {
    if(Serial){
        String command = Serial.readStringUntil("\n");
        if (command.substring(0,3).equals("GET")) {
            Serial.write(read_temperature());
        }
        else if (command.substring(0,3).equals("SET")) {
            if (command.charAt(4) == '1') {
                OvenOn();
            } else if (command.charAt(4) == '0') {
                OvenOff();
            }
        }
    }
}
