// OK, this is a hack while I'm waiting for the lisp version to finish running.
// clang -Wall -g -o adv19-12 adv19-12.c
// clang -Wall -Ofast -o adv19-12 adv19-12.c

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <err.h>
#include <ctype.h>
#include <limits.h>
#include <math.h>
#include <assert.h>
#include <locale.h>

#define LENGTHOF(X) (sizeof(X)/sizeof((X)[0]))

int verbose = 0;
char *input_file = "adv19-12.input";
int part = 1;

typedef struct moon_s {
    struct {
	int16_t x;
	int16_t y;
	int16_t z;
    } pos;
    struct {
	int16_t x;
	int16_t y;
	int16_t z;
    } vel;
} moon_t;

moon_t moonv[4];
int moonv_len;

void add_moon(moon_t *moons, int *count_p, int x, int y, int z, int dx, int dy, int dz)
{
    printf("Add moon %d: (%d,%d,%d)/(%d,%d,%d)\n", *count_p, x, y, z, dx, dy, dz);
    moons[*count_p].pos.x = x;
    moons[*count_p].pos.y = y;
    moons[*count_p].pos.z = z;
    moons[*count_p].vel.x = dx;
    moons[*count_p].vel.y = dy;
    moons[*count_p].vel.z = dz;
    (*count_p)++;
}

void read_input(char *filename)
{
    FILE *infp;
    char buf[80];

    infp = fopen(filename, "r");
    if (NULL == infp)
	err(1, "fopen(%s)", input_file);
    while (fgets(buf, sizeof buf, infp) != NULL) {
	int x, y, z, dx=0, dy=0, dz=0;
	char *p;
	if (3 != sscanf(buf, "<x=%d, y=%d, z=%d>", &x, &y, &z))
	    err(1, "Bad format: %s", buf);
	if ((p = strchr(buf, '/'))) {
	    if (3 != sscanf(p+1, "<dx=%d, dy=%d, dz=%d>", &dx, &dy, &dz))
		err(1, "Bad velocity format: %s", buf);
	}
	add_moon(moonv, &moonv_len, x, y, z, dx, dy, dz);
    }
    fclose(infp);
}

int total_energy(moon_t *state)
{
    int sum = 0, i;

    for (i = 0; i < LENGTHOF(moonv); i++) {
	sum += (abs(state[i].pos.x) + abs(state[i].pos.y) + abs(state[i].pos.z))
	    * (abs(state[i].vel.x) + abs(state[i].vel.y) + abs(state[i].vel.z));
    }
    return sum;
}

void do_one_timestep(moon_t *moon)
{
    int num_moons = LENGTHOF(moonv), i, j;
    // first, update the velocities
    for (i = 0; i < num_moons; i++) {
	for (j = 0; j < num_moons; j++) {
	    if (i != j) {
		if (moon[i].pos.x != moon[j].pos.x) {
		    moon[i].vel.x += moon[i].pos.x > moon[j].pos.x ? -1 : 1;
		}
		if (moon[i].pos.y != moon[j].pos.y) {
		    moon[i].vel.y += moon[i].pos.y > moon[j].pos.y ? -1 : 1;
		}
		if (moon[i].pos.z != moon[j].pos.z) {
		    moon[i].vel.z += moon[i].pos.z > moon[j].pos.z ? -1 : 1;
		}
	    }
	}
    }
    // Now, update the positions
    for (i = 0; i < num_moons; i++) {
	moon[i].pos.x += moon[i].vel.x;
	moon[i].pos.y += moon[i].vel.y;
	moon[i].pos.z += moon[i].vel.z;
    }
}

void print_state(char *msg, uint64_t state_index, moon_t *state)
{
    int j;
    printf("%s %'lu:\n", msg, state_index);
    for (j=0; j < LENGTHOF(moonv); j++) {
	printf("  (%d,%d,%d)/(%d,%d,%d)\n",
	       state[j].pos.x, state[j].pos.y, state[j].pos.z,
	       state[j].vel.x, state[j].vel.y, state[j].vel.z);
    }
}

uint64_t gcd(uint64_t n1, uint64_t n2)
{
    while (n2 != 0) {
       uint64_t t = n2; 
       n2 = n1 % n2;
       n1 = t;
    }
    return n1;
}

uint64_t lcm(uint64_t n1, uint64_t n2)
{
    return n1 * n2 / gcd(n1, n2);
}

int main(int argc, char **argv)
{
    int c;
    uint64_t state_index = 1, x_cycle = 0, y_cycle = 0, z_cycle = 0;

    setlocale(LC_NUMERIC, "");
    while ((c = getopt(argc, argv, "v:f:p:")) != EOF) {
	switch (c) {
	case 'f':
	    input_file = strdup(optarg);
	    break;
	case 'v':
	    verbose = atoi(optarg);
	    break;
	case 'p':
	    part = atoi(optarg);
	    break;
	}
    }

    read_input(input_file);
    
    if (1 == part) {
	int i;
	for (i = 0; i < 1000; i++) {
	    do_one_timestep(moonv);
	}
	printf("Total energy: %d\n", total_energy(moonv));
    }

    if (2 == part) {
	static moon_t initial_state[LENGTHOF(moonv)];
	memcpy(initial_state, moonv, sizeof(moonv));
	do_one_timestep(moonv);
	state_index++;

#define SAME_ON_AXIS(A)							\
	({								\
	    int i, result = 1;						\
	    for (i = 0; i < LENGTHOF(moonv); i++) {			\
		if (initial_state[i].pos.A != moonv[i].pos.A  ||	\
		    initial_state[i].vel.A != moonv[i].vel.A) {		\
		    result = 0;						\
		    break;						\
		}							\
	    }								\
	    result; })
	
	while (0 == x_cycle  ||  0 == y_cycle  ||  0 == z_cycle) {
	    if (verbose > 1) {
		print_state("State", state_index, moonv);
	    }

	    if (0 == x_cycle && SAME_ON_AXIS(x)) {
		printf("X cycle at iteration %lu\n", state_index);
		x_cycle = state_index;
	    }
	    if (0 == y_cycle && SAME_ON_AXIS(y)) {
		printf("Y cycle at iteration %lu\n", state_index);
		y_cycle = state_index;
	    }
	    if (0 == z_cycle && SAME_ON_AXIS(z)) {
		printf("Z cycle at iteration %lu\n", state_index);
		z_cycle = state_index;
	    }
	    
	    state_index++;
	    do_one_timestep(moonv);
	}
	printf("Cycle is LCM(%lu,%lu,%lu) = %lu\n", x_cycle, y_cycle, z_cycle,
	       lcm(x_cycle-1, lcm(y_cycle-1, z_cycle-1)));
    }
    
    return 0;
}
