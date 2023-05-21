#if defined(i386_HOST_ARCH) || defined(x86_64_HOST_ARCH) \
    || ((defined(arm_HOST_ARCH) || defined(aarch64_HOST_ARCH)) \
        && defined(__ARM_FEATURE_UNALIGNED)) \
    || defined(powerpc_HOST_ARCH) || defined(powerpc64_HOST_ARCH) \
    || defined(powerpc64le_HOST_ARCH)
/*
Not all architectures are forgiving of unaligned accesses; whitelist ones
which are known not to trap (either to the kernel for emulation, or crash).
*/
#define HS_UNALIGNED_POKES_OK 1
#else
#define HS_UNALIGNED_POKES_OK 0
#warning "tell me which CI jobs reach this"
#endif


#define HS_UNALIGNED_ByteArray_OPS_OK \
  MIN_VERSION_base(4,12,0) \
  && (MIN_VERSION_base(4,16,1) || UNALIGNED_POKES_OK)
/*
The unaligned ByteArray# primops became available with base-4.12.0,
but require an unaligned-friendly host architecture to be safe to use
until ghc-9.2.2; see https://gitlab.haskell.org/ghc/ghc/-/issues/21015
*/
