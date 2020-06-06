#include <stdlib.h>
#include <math.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

typedef struct {
  double r;
  double i;
} complex_value;


static void complex_square(complex_value *result, complex_value *a) {
  result->r = a->r * a->r - a->i * a->i;
  result->i = 2 * a->r * a->i;
}

static double complex_modulo_squared(complex_value *a) {
  return (a->r * a->r) + (a->i * a->i);
}

static void complex_add(complex_value *result, complex_value *a, complex_value *b)
{
  result->r = a->r + b->r;
  result->i = a->i + b->i;
}

static void extract_value(emacs_env *env, emacs_value in, complex_value *out)
{
  emacs_value car = env->funcall(env, env->intern(env, "car"), 1, &in);
  out->r = env->extract_float(env, car);
  emacs_value cdr = env->funcall(env, env->intern(env, "cdr"), 1, &in);
  out->i = env->extract_float(env, cdr);
}

// TODO/FIXME how about memory?
static emacs_value mandelbrot_complex_add(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void) data;
  (void) nargs;
  
  complex_value a;
  extract_value(env, args[0], &a);
  complex_value b;
  extract_value(env, args[1], &b);
  
  complex_add(&a, &a, &b);

  emacs_value results[2];
  results[0] = env->make_float(env, a.r);
  results[1] = env->make_float(env, a.i);

  return env->funcall(env, env->intern(env, "cons"), 2, results);
}

static void create_mandelbrot_complex_add(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "mandelbrot-c/+");
  args[1] = env->make_function(env, 2, 2, mandelbrot_complex_add, "Complex addition. Accepts 2 cons cells as the complex numbers.", NULL);  
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

int emacs_module_init(struct emacs_runtime *runtime) {
  create_mandelbrot_complex_add(runtime->get_environment(runtime));
  return 0;
}
