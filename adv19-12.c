// OK, this is a hack while I'm waiting for the lisp version to finish running.
// clang -Wall -g -o adv19-12 adv19-12.c

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
char *resume_file = NULL;
int part = 1;

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
int moonv_len;
moon_t resume_moonv[4];
int resume_moonv_len;

void add_moon(moon_t *moons, int *count_p, int x, int y, int z, int dx, int dy, int dz)
{
    printf("Add moon %d: (%d,%d,%d)/(%d,%d,%d)\n", *count_p, x, y, z, dx, dy, dz);
    moons[*count_p].pos.all = 0;
    moons[*count_p].vel.all = 0;
    moons[*count_p].pos.pos.x = x;
    moons[*count_p].pos.pos.y = y;
    moons[*count_p].pos.pos.z = z;
    moons[*count_p].vel.vel.x = dx;
    moons[*count_p].vel.vel.y = dy;
    moons[*count_p].vel.vel.z = dz;
    (*count_p)++;
}

void read_input(int is_resume_file, char *filename, uint64_t *step_number_p)
{
    FILE *infp;
    char buf[80];
    *step_number_p = 1;

    infp = fopen(filename, "r");
    if (NULL == infp)
	err(1, "fopen(%s)", input_file);
    while (fgets(buf, sizeof buf, infp) != NULL) {
	int x, y, z, dx=0, dy=0, dz=0;
	char *p;
	if (buf[0] == '#')
	    continue;
	if (is_resume_file && isdigit(buf[0])) {
	    *step_number_p = strtoull(buf, NULL, 0);
	    continue;
	}
	if (3 != sscanf(buf, "<x=%d, y=%d, z=%d>", &x, &y, &z))
	    err(1, "Bad format: %s", buf);
	if ((p = strchr(buf, '/'))) {
	    if (3 != sscanf(p+1, "<dx=%d, dy=%d, dz=%d>", &dx, &dy, &dz))
		err(1, "Bad velocity format: %s", buf);
	}
	add_moon(is_resume_file ? resume_moonv : moonv,
		 is_resume_file ? &resume_moonv_len : &moonv_len,
		 x, y, z, dx, dy, dz);
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

#if 0
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
#else
# define states_equal(s0,s1)			\
    (s0[0].pos.all == s1[0].pos.all &&		\
     s0[1].pos.all == s1[1].pos.all &&		\
     s0[2].pos.all == s1[2].pos.all &&		\
     s0[3].pos.all == s1[3].pos.all)
#endif
    
int main(int argc, char **argv)
{
    int c;
    uint64_t state_index = 1;
    int max_iterations = 0;

    setlocale(LC_NUMERIC, "");
    while ((c = getopt(argc, argv, "v:f:p:m:r:")) != EOF) {
	switch (c) {
	case 'f':
	    input_file = strdup(optarg);
	    break;
	case 'r':
	    resume_file = strdup(optarg);
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

    read_input(0, input_file, &state_index);
    if (resume_file)
	read_input(1, resume_file, &state_index);
    
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
	if (resume_file) {
	    memcpy(moonv, resume_moonv, sizeof(moonv));
	} else {
	    do_one_timestep(moonv);
	    state_index++;
	}
	while (!states_equal(initial_state, moonv)  &&  (0 == max_iterations  ||  state_index < max_iterations)) {
	    if (0 == (state_index % 10000000) || (verbose > 1)) {
		print_state("State", state_index, moonv);
	    }
	    state_index++;
	    do_one_timestep(moonv);
	}
	if (states_equal(initial_state, moonv))
	    printf("Sequence repeats at step %lu.\n", state_index);
	print_state("State", state_index, moonv);
    }
    
    return 0;
}
