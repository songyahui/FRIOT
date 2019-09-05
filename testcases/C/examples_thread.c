#include<stdio.h>
#include<string.h>
#include<pthread.h>
#include<stdlib.h>
#include<unistd.h>
#include "wiringPi.h"

#define TempSensor 0
#define LCD 1
#define LED 3

void setup2() {
        pinMode (LED, OUTPUT) ;         // aka BCM_GPIO pin 17
}

void loop2() {
    printf ("Raspberry Pi blink\n") ;
    digitalWrite (0, 1) ;       // On
    delay (500) ;               // mS
    digitalWrite (0, 0) ;       // Off
    delay (500) ;
}

void setup1() {
        pinMode(TempSensor, INPUT);
        pinMode(LCD, OUTPUT); // actually needs more parameters
}

void loop1() {
    printf ("show lcd\n") ;
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
    //printf ( "%s",data);
    delay(30);
}

void* thread1(void *arg){
        setup1();
        while(1) loop1();
        return 0;
}

void* thread2(void *arg){
        setup2();
        printf ("thread2\n") ;
        while(1) loop2();
        return 0;
}

pthread_t tid[2];

int main(void)
{
    int i = 0;
    int err1, err2;

        err1 = pthread_create(&(tid[0]), NULL, &thread1, NULL);
        if (err1 != 0)
            printf("\ncan't create thread :[%s]", strerror(err1));
        else
            printf("\n Thread created successfully\n");

        err2 = pthread_create(&(tid[1]), NULL, &thread2, NULL);
        if (err2 != 0)
            printf("\ncan't create thread :[%s]", strerror(err2));
        else
            printf("\n Thread created successfully\n");

    sleep(5000);
    return 0;
}