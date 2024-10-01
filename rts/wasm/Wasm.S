#include "ghcconfig.h"
#include "rts/Constants.h"
#include "DerivedConstants.h"

#if SIZEOF_VOID_P == 4
#define W_ i32
#else
#define W_ i64
#endif

#if !defined(__PIC__)

	.hidden __R1
	.globl __R1
	.section .data.__R1,"",@
	.globaltype __R1, W_
__R1:

	.hidden __R2
	.globl __R2
	.section .data.__R2,"",@
	.globaltype __R2, W_
__R2:

	.hidden __R3
	.globl __R3
	.section .data.__R3,"",@
	.globaltype __R3, W_
__R3:

	.hidden __R4
	.globl __R4
	.section .data.__R4,"",@
	.globaltype __R4, W_
__R4:

	.hidden __R5
	.globl __R5
	.section .data.__R5,"",@
	.globaltype __R5, W_
__R5:

	.hidden __R6
	.globl __R6
	.section .data.__R6,"",@
	.globaltype __R6, W_
__R6:

	.hidden __R7
	.globl __R7
	.section .data.__R7,"",@
	.globaltype __R7, W_
__R7:

	.hidden __R8
	.globl __R8
	.section .data.__R8,"",@
	.globaltype __R8, W_
__R8:

	.hidden __R9
	.globl __R9
	.section .data.__R9,"",@
	.globaltype __R9, W_
__R9:

	.hidden __R10
	.globl __R10
	.section .data.__R10,"",@
	.globaltype __R10, W_
__R10:

	.hidden __F1
	.globl __F1
	.section .data.__F1,"",@
	.globaltype __F1, f32
__F1:

	.hidden __F2
	.globl __F2
	.section .data.__F2,"",@
	.globaltype __F2, f32
__F2:

	.hidden __F3
	.globl __F3
	.section .data.__F3,"",@
	.globaltype __F3, f32
__F3:

	.hidden __F4
	.globl __F4
	.section .data.__F4,"",@
	.globaltype __F4, f32
__F4:

	.hidden __F5
	.globl __F5
	.section .data.__F5,"",@
	.globaltype __F5, f32
__F5:

	.hidden __F6
	.globl __F6
	.section .data.__F6,"",@
	.globaltype __F6, f32
__F6:

	.hidden __D1
	.globl __D1
	.section .data.__D1,"",@
	.globaltype __D1, f64
__D1:

	.hidden __D2
	.globl __D2
	.section .data.__D2,"",@
	.globaltype __D2, f64
__D2:

	.hidden __D3
	.globl __D3
	.section .data.__D3,"",@
	.globaltype __D3, f64
__D3:

	.hidden __D4
	.globl __D4
	.section .data.__D4,"",@
	.globaltype __D4, f64
__D4:

	.hidden __D5
	.globl __D5
	.section .data.__D5,"",@
	.globaltype __D5, f64
__D5:

	.hidden __D6
	.globl __D6
	.section .data.__D6,"",@
	.globaltype __D6, f64
__D6:

	.hidden __L1
	.globl __L1
	.section .data.__L1,"",@
	.globaltype __L1, i64
__L1:

	.hidden __Sp
	.globl __Sp
	.section .data.__Sp,"",@
	.globaltype __Sp, W_
__Sp:

	.hidden __SpLim
	.globl __SpLim
	.section .data.__SpLim,"",@
	.globaltype __SpLim, W_
__SpLim:

	.hidden __Hp
	.globl __Hp
	.section .data.__Hp,"",@
	.globaltype __Hp, W_
__Hp:

	.hidden __HpLim
	.globl __HpLim
	.section .data.__HpLim,"",@
	.globaltype __HpLim, W_
__HpLim:

#endif
