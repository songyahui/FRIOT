#include <stdio.h>
#include <string.h>
#include <pthread.h>
#include <stdlib.h>
#include <unistd.h>
#include "wiringPi.h"

typedef struct _Model
{
    char *lcd_2;
    char *lcd_show;
    int mode_lcd;
    int env_temprature;
    int led_3;
    int ispeoplein;
    char *lcd_4;
    char *lcd_show1;
    int peoplecount;
    int env_motion;

} Model;
Model model = {"nothing", "nothing", 0, 0, 0, 0, "nothing", "nothing", 0, 0};
#define LCD_2 2

void *thread0(void *arg)
{
    pinMode(LCD_2, OUTPUT);
    while (1)
    {
        lcdPuts(LCD_2, model.lcd_2);
        delay(100);
    }
    return NULL;
}
char *lcd_show_meth(int a, int b)
{
    if (a)
    {
        return toStr(b);
    }
    else
    {
        return "null";
    }
}

void *thread1(void *arg)
{
    while (1)
    {
        char *value = lcd_show_meth(model.mode_lcd, model.env_temprature);

        model.lcd_show = value;
        delay(100);
    }
    return NULL;
}
int mode_LCD_meth(int a)
{
    if (a)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

void *thread2(void *arg)
{
    while (1)
    {
        int value = mode_LCD_meth(model.ispeoplein);

        model.mode_lcd = value;
        delay(100);
    }
    return NULL;
}
#define ENV_TEMPRATURE 1

void *thread3(void *arg)
{
    pinMode(ENV_TEMPRATURE, INPUT);
    while (1)
    {
        int value = digitalRead(ENV_TEMPRATURE);
        if (value != model.env_temprature)
        {
            model.env_temprature = value;
        }
        delay(100);
    }
    return NULL;
}
#define LED_3 3

void *thread4(void *arg)
{
    pinMode(LED_3, OUTPUT);
    while (1)
    {
        digitalWrite(LED_3, model.led_3);
        delay(100);
    }
    return NULL;
}
int isPeopleIn_meth(int a)
{
    if (a)
    {
        return 1;
    }
    else
    {
        return 0;
    }
}

void *thread5(void *arg)
{
    while (1)
    {
        int value = isPeopleIn_meth(model.env_motion);

        model.ispeoplein = value;
        delay(100);
    }
    return NULL;
}
#define LCD_4 4

void *thread6(void *arg)
{
    pinMode(LCD_4, OUTPUT);
    while (1)
    {
        lcdPuts(LCD_4, model.lcd_4);
        delay(100);
    }
    return NULL;
}
char *lcd_show1_meth(int a)
{
    return toStr(a);
}

void *thread7(void *arg)
{
    while (1)
    {
        char *value = lcd_show1_meth(model.peoplecount);

        model.lcd_show1 = value;
        delay(100);
    }
    return NULL;
}

void *thread8(void *arg)
{
    while (1)
    {
        delay(100);
    }
    return NULL;
}
#define ENV_MOTION 0

void *thread9(void *arg)
{
    pinMode(ENV_MOTION, INPUT);
    while (1)
    {
        int value = digitalRead(ENV_MOTION);
        if (value != model.env_motion)
        {
            model.env_motion = value;
        }
        delay(100);
    }
    return NULL;
}
pthread_t tid[10];
int main(void)
{
    int err9;
    err9 = pthread_create(&(tid[9]), NULL, &thread9, NULL);
    if (err9 != 0)
        printf("can't create thread :[%s]", strerror(err9));
    else
        printf(" Thread 9 created successfully");
    int err8;
    err8 = pthread_create(&(tid[8]), NULL, &thread8, NULL);
    if (err8 != 0)
        printf("can't create thread :[%s]", strerror(err8));
    else
        printf(" Thread 8 created successfully");
    int err7;
    err7 = pthread_create(&(tid[7]), NULL, &thread7, NULL);
    if (err7 != 0)
        printf("can't create thread :[%s]", strerror(err7));
    else
        printf(" Thread 7 created successfully");
    int err6;
    err6 = pthread_create(&(tid[6]), NULL, &thread6, NULL);
    if (err6 != 0)
        printf("can't create thread :[%s]", strerror(err6));
    else
        printf(" Thread 6 created successfully");
    int err5;
    err5 = pthread_create(&(tid[5]), NULL, &thread5, NULL);
    if (err5 != 0)
        printf("can't create thread :[%s]", strerror(err5));
    else
        printf(" Thread 5 created successfully");
    int err4;
    err4 = pthread_create(&(tid[4]), NULL, &thread4, NULL);
    if (err4 != 0)
        printf("can't create thread :[%s]", strerror(err4));
    else
        printf(" Thread 4 created successfully");
    int err3;
    err3 = pthread_create(&(tid[3]), NULL, &thread3, NULL);
    if (err3 != 0)
        printf("can't create thread :[%s]", strerror(err3));
    else
        printf(" Thread 3 created successfully");
    int err2;
    err2 = pthread_create(&(tid[2]), NULL, &thread2, NULL);
    if (err2 != 0)
        printf("can't create thread :[%s]", strerror(err2));
    else
        printf(" Thread 2 created successfully");
    int err1;
    err1 = pthread_create(&(tid[1]), NULL, &thread1, NULL);
    if (err1 != 0)
        printf("can't create thread :[%s]", strerror(err1));
    else
        printf(" Thread 1 created successfully");
    int err0;
    err0 = pthread_create(&(tid[0]), NULL, &thread0, NULL);
    if (err0 != 0)
        printf("can't create thread :[%s]", strerror(err0));
    else
        printf(" Thread 0 created successfully");
    sleep(5000);
    return 0;
}
