// OK, this is a hack while I'm waiting for the lisp version to finish running.
// clang -Wall -g -o adv19-12 adv19-12.c

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <err.h>
#include <limits.h>
#include <math.h>
#include <assert.h>
#include <locale.h>

#define LENGTHOF(X) (sizeof(X)/sizeof((X)[0]))

int verbose = 0;
char *input_file = "adv19-12.input";
int part = 1;
int max_iterations = 0;

typedef struct moon_s {
    union {
	struct {
	    int16_t x;
	    int16_t y;
	    int16_t z;
	} pos;
	int64_t all;
    } pos;
    union {
	struct {
	    int16_t x;
	    int16_t y;
	    int16_t z;
	} vel;
	int64_t all;
    } vel;
} moon_t;

moon_t moonv[4];

void add_moon(int x, int y, int z)
{
    static int i = 0;

    assert(i < LENGTHOF(moonv));
    printf("Add moon %d: (%d,%d,%d)\n", i, x, y, z);
    moonv[i].pos.all = 0;
    moonv[i].vel.all = 0;
    moonv[i].pos.pos.x = x;
    moonv[i].pos.pos.y = y;
    moonv[i].pos.pos.z = z;
    i++;
}

void read_input(void)
{
    FILE *infp;
    char buf[80];

    infp = fopen(input_file, "r");
    if (NULL == infp)
	err(1, "fopen(%s)", input_file);
    while (fgets(buf, sizeof buf, infp) != NULL) {
	int x, y, z;
	if (3 != sscanf(buf, "<x=%d, y=%d, z=%d>", &x, &y, &z))
	    err(1, "Bad format: %s", buf);
	add_moon(x, y, z);
    }
    fclose(infp);
}

int total_energy(moon_t *state)
{
    int sum = 0, i;

    for (i = 0; i < LENGTHOF(moonv); i++) {
	sum += (abs(state[i].pos.pos.x) + abs(state[i].pos.pos.y) + abs(state[i].pos.pos.z))
	    * (abs(state[i].vel.vel.x) + abs(state[i].vel.vel.y) + abs(state[i].vel.vel.z));
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
		if (moon[i].pos.pos.x != moon[j].pos.pos.x) {
		    moon[i].vel.vel.x += moon[i].pos.pos.x > moon[j].pos.pos.x ? -1 : 1;
		}
		if (moon[i].pos.pos.y != moon[j].pos.pos.y) {
		    moon[i].vel.vel.y += moon[i].pos.pos.y > moon[j].pos.pos.y ? -1 : 1;
		}
		if (moon[i].pos.pos.z != moon[j].pos.pos.z) {
		    moon[i].vel.vel.z += moon[i].pos.pos.z > moon[j].pos.pos.z ? -1 : 1;
		}
	    }
	}
    }
    // Now, update the positions
    for (i = 0; i < num_moons; i++) {
	moon[i].pos.pos.x += moon[i].vel.vel.x;
	moon[i].pos.pos.y += moon[i].vel.vel.y;
	moon[i].pos.pos.z += moon[i].vel.vel.z;
    }
}

void print_state(char *msg, uint64_t state_index, moon_t *state)
{
    int j;
    printf("%s %'lu:\n", msg, state_index);
    for (j=0; j < LENGTHOF(moonv); j++) {
	printf("  (%d,%d,%d)/(%d,%d,%d)"
#if 0
	       " [%#018lx %#018lx]"
#else
	       ""
#endif
	       "\n",
	       state[j].pos.pos.x,  state[j].pos.pos.y,  state[j].pos.pos.z,
	       state[j].vel.vel.x, state[j].vel.vel.y, state[j].vel.vel.z
#if 0
	       ,state[j].pos.all, state[j].vel.all
#endif
	    );
    }
}

int states_equal(moon_t *s0, moon_t *s1)
{
    int i;
    for (i = 0; i < LENGTHOF(moonv); i++) {
#if 0
	if (s0[i].pos.pos.x != s1[i].pos.pos.x || s0[i].pos.pos.y != s1[i].pos.pos.y || s0[i].pos.pos.z != s1[i].pos.pos.z)
	    return 0;
	if (s0[i].vel.vel.x != s1[i].vel.vel.x || s0[i].vel.vel.y != s1[i].vel.vel.y || s0[i].vel.vel.z != s1[i].vel.vel.z)
	    return 0;
#else
	if (s0[i].pos.all != s1[i].pos.all || s0[i].vel.all != s1[i].vel.all)
	    return 0;
#endif
    }
    return 1;
}
    
int main(int argc, char **argv)
{
    int c;

    setlocale(LC_NUMERIC, "");
    while ((c = getopt(argc, argv, "v:f:p:m:")) != EOF) {
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
	case 'm':
	    max_iterations = atoi(optarg);
	    break;
	}
    }

    read_input();
    
    if (1 == part) {
	int i;
	for (i = 0; i < 1000; i++) {
	    do_one_timestep(moonv);
	}
	printf("Total energy: %d\n", total_energy(moonv));
    }

    if (2 == part) {
	moon_t slow_state[LENGTHOF(moonv)];
	moon_t fast_state[LENGTHOF(moonv)];
	uint64_t slow_index = 0;
	uint64_t fast_index = 1;
	int i;
	for (i = 0; i < LENGTHOF(moonv); i++) {
	    slow_state[i] = moonv[i];
	    fast_state[i] = moonv[i];
	}
	do_one_timestep(fast_state);
	while (!states_equal(slow_state, fast_state)  &&  (0 == max_iterations  ||  fast_index < max_iterations)) {
	    if (0 == (fast_index % 10000000) || (verbose > 1)) {
		print_state("Fast", fast_index, fast_state);
	    }
	    if (0 == (fast_index % 2)) {
		slow_index++;
		do_one_timestep(slow_state);
	    }
	    fast_index++;
	    do_one_timestep(fast_state);
	}
	if (0 == max_iterations  ||  fast_index < max_iterations)
	    printf("Sequence repeats at step %lu/%lu.  Cycle-length %lu.\n",
		   fast_index, slow_index, fast_index - slow_index);
	print_state("Fast", fast_index, fast_state);
    }
    
    return 0;
}
