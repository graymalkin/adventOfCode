#include <string.h>
#include <stdlib.h>
#include <stdio.h>

typedef struct  {
    int pos[3];
    int vel[3];
} moon_t;

moon_t moons[4] = {
    { {14, 15,  -2}, {0,0,0} }, /* IO */
    { {17, -3,   4}, {0,0,0} }, /* EUROPA */
    { { 6, 12, -13}, {0,0,0} }, /* GANYMEDE */
    { {-2, 10,  -8}, {0,0,0} } /* CALLISTO */
};

#define X 0
#define Y 1
#define Z 2

void component_gravity(int* results, int x, int y, int vx, int vy) {
    if (x < y) { results[0] = vx+1; results[1] = vy-1; }
    else if (x > y) { results[0] = vx-1; results[1] = vy+1; }
    else { results[0] = vx; results[1] = vy; }
}

void gravity(int component, moon_t * m1, moon_t * m2) {
    int results[2];
    component_gravity(results, m1->pos[component], m2->pos[component], m1->vel[component], m2->vel[component]);
    m1->vel[component] = results[0];
    m2->vel[component] = results[1];
}

void update_position(int component, moon_t * m) {
    m->pos[component] = m->pos[component] + m->vel[component];
}

void step (int component) {
    gravity(component, &moons[0], &moons[1]);
    gravity(component, &moons[0], &moons[2]);
    gravity(component, &moons[0], &moons[3]);

    gravity(component, &moons[1], &moons[2]);
    gravity(component, &moons[1], &moons[3]);

    gravity(component, &moons[2], &moons[3]);

    update_position(component, &moons[0]);
    update_position(component, &moons[1]);
    update_position(component, &moons[2]);
    update_position(component, &moons[3]);
}

int period_component(int component, moon_t * moons, moon_t * start) {
    long period = 0;
    for (long i = 1; ; i++) {
        step(component);
        if (moons[0].pos[component] == start[0].pos[component] && moons[0].vel[component] == start[0].vel[component] &&
            moons[1].pos[component] == start[1].pos[component] && moons[1].vel[component] == start[1].vel[component] &&
            moons[2].pos[component] == start[2].pos[component] && moons[2].vel[component] == start[2].vel[component] &&
            moons[3].pos[component] == start[3].pos[component] && moons[3].vel[component] == start[3].vel[component]
         ) {
             period = i;
             printf("period: %ld\n", i);
             break;
         }
    }
    return period;
}

int main () {
    moon_t * start = malloc(sizeof(moon_t) * 4);
    memcpy(start, moons, sizeof(moon_t) * 4);
    long period_x = period_component(X, moons, start);

    memcpy(moons, start, sizeof(moon_t) * 4);
    long period_y = period_component(Y, moons, start);

    memcpy(moons, start, sizeof(moon_t) * 4);
    long period_z = period_component(Z, moons, start);
   
    printf("x: %ld, y: %ld, z: %ld\n", period_x, period_y, period_z);
    
    return 0;
}
