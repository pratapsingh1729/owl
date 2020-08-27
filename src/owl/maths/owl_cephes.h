 /*
 *   This file was automatically generated by version 1.7 of cextract.
 *   Manual editing not recommended.
 *
 *   Edited for use by cephesmodule.c by Travis Oliphant
 *
 *   Created: Fri Mar 31 19:17:33 1995
 */

#ifndef CEPHES_H
#define CEPHES_H

#include "cephes_names.h"

#ifdef __cplusplus
extern "C" {
#endif

extern int airy ( double x, double *ai, double *aip, double *bi, double *bip );

extern double bdtrc ( int k, int n, double p );
extern double bdtr ( int k, int n, double p );
extern double bdtri ( int k, int n, double y );

extern double beta ( double a, double b );
extern double lbeta ( double a, double b );

extern double btdtr ( double a, double b, double x );

extern double cbrt ( double x );
/*
extern double chbevl ( double x, void *P, int n );
*/
extern double chdtrc ( double df, double x );
extern double chdtr ( double df, double x );
extern double chdtri ( double df, double y );
extern double dawsn ( double xx );
/*
extern void eigens ( double A[], double RR[], double E[], int N );
*/
extern double ellie ( double phi, double m );
extern double ellik ( double phi, double m );
extern double ellpe ( double x );

extern int ellpj ( double u, double m, double *sn, double *cn, double *dn, double *ph );
extern double ellpk ( double x );
extern double exp10 ( double x );
extern double exp1m ( double x );
extern double exp2 ( double x );

extern double expn ( int n, double x );

/*
extern double fabs ( double x );
extern double fac ( int i );
*/

extern double fdtrc ( double a, double b, double x );
extern double fdtr ( double a, double b, double x );
extern double fdtri ( double a, double b, double y );

/*
extern int fftr ( double x[], int m0, double sine[] );
extern double frexp ( double x, int *pw2 );
*/
/*
extern double ldexp ( double x, int pw2 );
*/

extern int fresnl ( double xxa, double *ssa, double *cca );
extern double Gamma ( double x );
extern double lgam ( double x );

extern double gdtr ( double a, double b, double x );
extern double gdtrc ( double a, double b, double x );
extern double gdtri ( double a, double b, double y );

/*
extern int gels ( double A[], double R[], int M, double EPS, double AUX[] );
*/
extern double hyp2f1 ( double a, double b, double c, double x );
extern double hyperg ( double a, double b, double x );
extern double hyp2f0 ( double a, double b, double x, int type, double *err );
extern double onef2 ( double a, double b, double c, double x, double *err );
extern double threef0 ( double a, double b, double c, double x, double *err );


extern double i0 ( double x );
extern double i0e ( double x );
extern double i1 ( double x );
extern double i1e ( double x );
extern double igamc ( double a, double x );
extern double igam ( double a, double x );
extern double igam_fac( double a, double x );
extern double igami ( double a, double y0 );

extern double incbet ( double aa, double bb, double xx );
extern double incbi ( double aa, double bb, double yy0 );

extern double iv ( double v, double x );
extern double j0 ( double x );
extern double y0 ( double x );
extern double j1 ( double x );
extern double y1 ( double x );

extern double jn ( int n, double x );
extern double jv ( double n, double x );
extern double k0 ( double x );
extern double k0e ( double x );
extern double k1 ( double x );
extern double k1e ( double x );
extern double kn ( int nn, double x );
/*
extern int levnsn ( int n, double r[], double a[], double e[], double refl[] );
#ifndef log2
extern double log2 ( double x );
#endif
*/
/*
extern long lrand ( void );
extern long lsqrt ( long x );
extern int mtherr ( char *name, int code );
extern double polevl ( double x, void *P, int N );
extern double p1evl ( double x, void *P, int N );
*/
extern double nbdtrc ( int k, int n, double p );
extern double nbdtr ( int k, int n, double p );
extern double nbdtri ( int k, int n, double p );

extern double ndtr ( double a );
extern double log_ndtr ( double a );
extern double erfc ( double a );
extern double erf ( double x );
extern double ndtri ( double y0 );

extern double pdtrc ( int k, double m );
extern double pdtr ( int k, double m );
extern double pdtri ( int k, double y );
/*
extern double pow ( double x, double y );
extern double powi ( double x, int nn );
*/
extern double psi ( double x );
/*
extern void revers ( double y[], double x[], int n );
 */
extern double rgamma ( double x );
extern double round ( double x );

/*
extern int sprec ( void );
extern int dprec ( void );
extern int ldprec ( void );
*/
extern int shichi ( double x, double *si, double *ci );
extern int sici ( double x, double *si, double *ci );
/*
extern double simpsn ( double f[], double delta );
extern int simq ( double A[], double B[], double X[], int n, int flag, int IPS[] );
*/
extern double radian ( double d, double m, double s );
/*
extern int sincos ( double x, double *s, double *c, int flg );
*/
extern double sindg ( double x );
extern double cosdg ( double x );
/*
extern double sinh ( double x );
*/
extern double spence ( double x );
/*
extern double sqrt ( double x );
*/
extern double stdtr ( int k, double t );
extern double stdtri ( int k, double p );


extern double struve ( double v, double x );
extern double yv ( double v, double x);
/*
extern double tan ( double x );
extern double cot ( double x );
*/
extern double tandg ( double x );
extern double cotdg ( double x );
/*
extern double tanh ( double x );
*/
extern double log1p ( double x );
extern double log1pmx( double x );
extern double expm1 ( double x );
extern double cosm1 ( double x );
extern double lgam1p ( double x );

extern double yn ( int n, double x );
extern double zeta ( double x, double q );
extern double zetac ( double x );

extern double smirnov (int n, double e );
extern double smirnovi (int n, double p );
extern double kolmogorov ( double x );
extern double kolmogi ( double p );

extern double lanczos_sum_expg_scaled( double x );

extern int airyf ( float xx, float *ai, float *aip, float *bi, float *bip );
extern float dawsnf ( float xx );
extern float i0f ( float xx );
extern float i0ef ( float xx );
extern float i1f ( float xx );
extern float i1ef ( float xx );
extern float ivf ( float v, float x );
extern float j0f ( float xx );
extern float j1f ( float xx );
extern float jvf ( float v, float x );
extern float jnf ( int nn, float xx );
extern float k0f ( float xx );
extern float k0ef ( float xx );
extern float k1f ( float xx );
extern float k1ef ( float xx );
extern float knf ( int nnn, float xx );
extern float y0f ( float xx );
extern float y1f ( float xx );
extern float ynf ( int nn, float xx );
extern float yvf ( float vv, float xx );

extern float struvef ( float vv, float xx );

#ifdef __cplusplus
}
#endif

#endif /* CEPHES_H */
