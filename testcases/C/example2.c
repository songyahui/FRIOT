#include <stdio.h>
#include "wiringPi.h"
#include <stdio.h>

#define LED 3

void setup() {
        pinMode (LED, OUTPUT) ;         // aka BCM_GPIO pin 17
}

void loop() {
    printf ("Raspberry Pi blink\n") ;
    digitalWrite (0, 1) ;       // On
    delay (5) ;               // mS
    digitalWrite (0, 0) ;       // Off
    delay (5) ;
}

int main() {
        if (wiringPiSetup() == -1) {
                return -1;         
        }
        setup();
        while(1) loop();
        return 0;
}