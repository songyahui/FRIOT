#include <stdio.h>
#include "wiringPi.h"
#include <time.h>
#include <string.h>

#define TempSensor 0
#define LCD 1

void setup() {
        pinMode(TempSensor, INPUT);
        pinMode(LCD, OUTPUT); // actually needs more parameters
}

void loop() {
    float temp = digitalRead(TempSensor);

    time_t timer;
    char buffer[26];
    struct tm* tm_info;

    time(&timer);
    tm_info = localtime(&timer);

    strftime(buffer, 26, "Time: %H:%M:%S", tm_info);

    char data[100] ;
    sprintf(data, "%f", temp); 
    strcat(data, buffer);
    strcat(data, "\n");
    lcdPuts(LCD, data);
    printf ( "%s",data);
    delay(3);
}

int main() {
        if (wiringPiSetup() == -1) {
                return -1;         
        }
        setup();
        while(1) loop();
        return 0;
}