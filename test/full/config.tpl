#define TARGET_ENDIANNESS big
#define HOST_ENDIANNESS little

#define PPC_SYSCALL_CODE(i, s) ((s)->GPR[0])
#define PPC_SYSCALL_MEM(s) ((s)->M)

#define PPC_SYSPARM_REG32_RCNT 4
#define PPC_SYSPARM_REG32_REG(s, i) ((s)->GPR[3 + (i)])
#define PPC_SYSPARM_REG32_SP(s) ((s)->GPR[1])
#define PPC_SYSPARM_REG32_RETURN(s, v)	{ (s)->GPR[3] = (v); }
