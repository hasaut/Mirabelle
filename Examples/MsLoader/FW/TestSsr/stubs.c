#include <stdint.h>
#include "mdl_MCBasic.h"


#ifndef MLX16
//	#include <math.h>

#endif


volatile uint8_t currentReconstructValid = 0x01;
volatile int16_t currentWvalid = 0x01FA;
volatile int16_t currentUvalid = 0x1555;
volatile int16_t currentVvalid = 0x0013;
volatile uint16_t sineIndexValid = 0x0012;

#ifndef MLX16
bool mdl_ph2C_automatedLeadAngleCalculation;
uint16_t motorRotSpeedRpmActual;
uint16_t motorRotSpeedRpmReport;
uint16_t volatile pwmDutyCycleActual;
uint16_t volatile OC_CFG, OC_CFG_KEY, OC_CS,OC_CS_KEY, OC_DAC,OC_DAC_KEY, OC_MON, PWM_CFG0, PWM_CFG0_KEY;
uint8_t volatile AWD, IWDT,IWDCTRL;
#endif

void mdl_ph2C_angleControllerUpdate(void);


void Sys_ResetCpu (void)
{
    LOOP_FOREVER();
}

void restartAwdg (void)
{
    setbit(AWD, AWD_ATT_BITNO);
}

uint8_t checkAwdgBoot (void)
{
    return ((AWD & AWD_BOOT) != 0);
}

uint8_t isAwdgAttention (void)
{
    return ((AWD & AWD_ATT) != 0);
}

void initIwdg (IwdgMode_t mode, IwdgClockDivider_t divider, uint8_t timeout)
{
    IWDT = timeout;
    IWDCTRL = mode | divider;
}

void restartIwdg (void)
{
    IWDT = 0u;
}

uint8_t checkIwdgBoot (void)
{
    return ((IWDCTRL & IWD_BOOT) != 0u);
}

uint8_t isIwdgWindowOpen (void)
{
    return ((IWDCTRL & IWD_WND) != 0u);
}

#ifndef MLX16

uint32_t mulU32_U16byU16(uint16_t multiplicand, uint16_t multiplier) {
	return multiplicand*multiplier;
}

uint32_t divU32_U32byU16(uint32_t n, uint16_t d){
	return n/d;
}

int32_t mulI32_I16byI16(int16_t multiplicand, int16_t multiplier){
	return multiplicand*multiplier;
}

int32_t  mulI32lo_I32byI16(int32_t multiplicand, int16_t multiplier){
	return multiplicand*multiplier;
}

uint16_t divU16_U32byU16(uint32_t dividend, uint16_t divisor){
	return dividend/divisor;
}

int16_t  mulQ15_Q15byQ15(int16_t multiplicand, int16_t multiplier){
	return multiplicand*multiplier;
}

int16_t atan2I16 (int16_t y, int16_t x){
	return 0;
	// Don't call atan2, we miss the math lib for clang
	//return atan2(y,x);
}

#endif

void mdl_Ph2C_MotorStop(enFailureState failureCode){}
void mdl_rsp_SetRotationalSpeedActualInRPM(uint16_t rot_speed){}

int main(void){

	mdl_ph2C_angleControllerUpdate();

	while (1) {}

	return 0;
}
