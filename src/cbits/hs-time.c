#ifdef __MACH__
#include <Availability.h>
#include <sys/time.h>

#ifdef __MAC_OS_X_VERSION_MIN_REQUIRED

#if __MAC_OS_X_VERSION_MIN_REQUIRED < 1012
//clock_gettime is not implemented on OSX
int clock_gettime(int clk_id, struct timespec* t) {
  struct timeval now;
  int rv = gettimeofday(&now, NULL);
  if (rv) return rv;
  t->tv_sec  = now.tv_sec;
  t->tv_nsec = now.tv_usec * 1000;
  return 0;
}
#else
#endif

#endif

#endif
