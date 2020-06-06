#include <stdlib.h>
#include <math.h>

#include <emacs-module.h>

int plugin_is_GPL_compatible;

typedef struct {
  double r;
  double i;
} complex_value;


static void complex_sqr(complex_value *result, complex_value *a) {
  result->r = a->r * a->r - a->i * a->i;
  result->i = 2 * a->r * a->i;
}

static void complex_add(complex_value *result, complex_value *a, complex_value *b)
{
  result->r = a->r + b->r;
  result->i = a->i + b->i;
}

static double extract_number_as_float(emacs_env *env, emacs_value v) {
  emacs_value number_as_float = env->funcall(env, env->intern(env, "float"), 1, &v);
  return env->extract_float(env, number_as_float);
}

static void extract_value(emacs_env *env, emacs_value in, complex_value *out)
{
  emacs_value car = env->funcall(env, env->intern(env, "car"), 1, &in);
  out->r = extract_number_as_float(env, car);
  emacs_value cdr = env->funcall(env, env->intern(env, "cdr"), 1, &in);
  out->i = extract_number_as_float(env, cdr);
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
  args[1] = env->make_function(env, 2, 2, mandelbrot_complex_add, "Addition in complex space. Accepts two cons cells as input.", NULL);  
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}


// TODO/FIXME not DRY at all!!
static emacs_value mandelbrot_complex_sqr(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void) data;
  (void) nargs;
  
  complex_value a;
  extract_value(env, args[0], &a);
  complex_value cresult;
  
  complex_sqr(&cresult, &a);

  emacs_value results[2];
  results[0] = env->make_float(env, cresult.r);
  results[1] = env->make_float(env, cresult.i);

  return env->funcall(env, env->intern(env, "cons"), 2, results);
}

static void create_mandelbrot_complex_sqr(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "mandelbrot-c/sqr");
  args[1] = env->make_function(env, 1, 1, mandelbrot_complex_sqr, "Square function in complex space. Accepts one cons cell as input.", NULL);  
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

static emacs_value mandelbrot_f(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void) data;
  (void) nargs;
  
  complex_value z;
  extract_value(env, args[0], &z);
  complex_value c;
  extract_value(env, args[1], &c);
  
  complex_value cresult;
  
  complex_sqr(&cresult, &z);
  complex_add(&cresult, &cresult, &c);

  emacs_value results[2];
  results[0] = env->make_float(env, cresult.r);
  results[1] = env->make_float(env, cresult.i);

  return env->funcall(env, env->intern(env, "cons"), 2, results);
}

static void create_mandelbrot_f(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "mandelbrot-c/f");
  args[1] = env->make_function(env, 2, 2, mandelbrot_f, "Mandelbrot function. Accepts two cons cells (z and c) as input.", NULL);  
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

static double complex_modulo_squared(complex_value *a) {
  return (a->r * a->r) + (a->i * a->i);
}

static emacs_value mandelbrot_modulo_squared(emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  (void) data;
  (void) nargs;
  
  complex_value a;
  extract_value(env, args[0], &a);
  
  double result = complex_modulo_squared(&a);

  return env->make_float(env, result);
}

static void create_mandelbrot_modulo_squared(emacs_env *env)
{
  emacs_value args[2];
  args[0] = env->intern(env, "mandelbrot-c/modulo-squared");
  args[1] = env->make_function(env, 1, 1, mandelbrot_modulo_squared, "|z|^2. Accepts one cons cells as input.", NULL);  
  env->funcall(env, env->intern(env, "defalias"), 2, args);
}

int emacs_module_init(struct emacs_runtime *runtime) {
  create_mandelbrot_complex_add(runtime->get_environment(runtime));
  create_mandelbrot_complex_sqr(runtime->get_environment(runtime));
  create_mandelbrot_f(runtime->get_environment(runtime));
  create_mandelbrot_modulo_squared(runtime->get_environment(runtime));
  return 0;
}
