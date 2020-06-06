#include <stdlib.h>
#include <math.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

struct complex_value {
  double r;
  double i;
};


static void complex_square(struct complex_value *result, struct complex_value *a) {
  result->r = a->r * a->r - a->i * a->i;
  result->i = 2 * a->r * a->i;
}

static double complex_modulo_squared(struct complex_value *a) {
  return (a->r * a->r) + (a->i * a->i);
}

static void complex_add(struct complex_value *result, struct complex_value *a, struct complex_value *b)
{
  result->r = a->r + b->r;
  result->i = a->i + b->i;
}

static emacs_value mandelbrot_complex_add(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void) nargs;
  (void) args;
  (void) data;
  return env->make_float(env, 42.0);
}

static void create_mandelbrot_complex_add(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "mandelbrot-c/+");
  args[1] = env->make_function(env, 0, 0, mandelbrot_complex_add, "Complex addition", NULL);
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

int emacs_module_init(struct emacs_runtime *runtime) {
  create_mandelbrot_complex_add(runtime->get_environment(runtime));
  return 0;
}
