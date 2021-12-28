/**
 * @file
 * @brief Motor control lead angle calculation module
 * @internal
 *
 * @copyright (C) 2015-2019 Melexis N.V.
 *
 * Melexis N.V. is supplying this code for use with Melexis N.V. processor based microcontrollers only.
 *
 * THIS SOFTWARE IS PROVIDED "AS IS".  NO WARRANTIES, WHETHER EXPRESS, IMPLIED OR STATUTORY,
 * INCLUDING, BUT NOT LIMITED TO, IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 * A PARTICULAR PURPOSE APPLY TO THIS SOFTWARE.  MELEXIS N.V. SHALL NOT IN ANY CIRCUMSTANCES,
 * BE LIABLE FOR SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES, FOR ANY REASON WHATSOEVER.
 *
 * @endinternal
 *
 * @ingroup application
 *
 * @details
 *
 * Motor control lead angle calculation module
 *
 */

/* ==========================================================================
 * Includes
 * ========================================================================== */
#include "lib_ocmonitor.h"

#include "AppConfig.h"
#include "mcl.h"
#include "sys_events.h"
#include "mcal.h"


#include "mdl_MCBasic.h"
#include "mdl_MCPhase2C.h"
#include "mdl_RotSpeed.h"
#include "mcl_ocsCmDef.h"
#include "mdl_PIDController.h"
#include "Interrupts.h"
#include "typelib_iv.h"
#ifdef MLX16
	#include "mathlib.h"
#endif
#ifndef MLX16
	#include "stubs.h"
#endif

/* ==========================================================================
 * Private type definitions, macros, defines
 * ========================================================================== */
#define DEGREES_TO_RADIANS  182
#define ANGLE_VARIATION     10
/* ==========================================================================
 * Declaration public functions
 * ========================================================================== */

/* ==========================================================================
 * Declaration private (static) functions
 * ========================================================================== */
/* ==========================================================================
 * Declaration private (static) variables
 * ========================================================================== */
STATIC int32_t mdl_ph2C_angleErrorSum  = 0;
STATIC int16_t mdl_ph2C_angleErrorDiff  = 0;
STATIC int16_t mdl_ph2C_angleErrorKeep  = 0;


/* ==========================================================================
 * Declaration public variables
 * ========================================================================== */

#pragma space dp

/* I/V angle method variables */
STATIC int16_t mdl_ph2C_leadAngle = 0;
STATIC int16_t mdl_ph2C_angleError;

#pragma space none


int16_t mdl_ph2C_leadAngleSetpoint = (FIXED_LEAD_ANGLE_VALUE * DEGREES_TO_RADIANS);


#define ANGLE_PID_DIVIDER  (4096u)
#define ANGLE_PID_I        (2U)
#define ANGLE_PID_P        (80U)
#define ANGLE_PID_D        (5U)

#define ANGLE_PID_I_LIMIT  ((int32_t)16384 * ANGLE_PID_DIVIDER)
#define ANGLE_PID_D_LIMIT  ((int16_t)(65536u / 24u))                          /* Limit D to 15 deg */

int16_t Kp_LA    = (int16_t)ANGLE_PID_P;
int16_t Ki_LA    = (int16_t)ANGLE_PID_I;
int16_t Kd_LA    = (int16_t)ANGLE_PID_D;

/* ==========================================================================
 * Implementation public functions
 * ========================================================================== */
static int32_t controllerY = 0;

/*
 * ----------------------------------------------------------------------------------------------
 * Common variable initialization for Phase2B start and commutation event
 *
 * @param  no call parameter
 * @return no return parameter
 */
void mdl_Ph2C_ResetLeadAngleControlVariables(uint16_t transitionSpeed)
{
    mdl_ph2C_angleErrorSum = (int32_t)divU32_U32byU16(mulU32_U16byU16(transitionSpeed,
                                                                      ANGLE_PID_DIVIDER), (uint16_t)Ki_LA);
    controllerY = 0;
    mdl_ph2C_angleErrorKeep = 0;
}

/**
 * ----------------------------------------------------------------------------------------------
 * Phase 2C - LA_angle PID controller to update motor speed
 *
 * @param - none
 * @return - no return parameter
 */

void mdl_ph2C_leadAngleController(int16_t angleDiff)
{
	int16_t speedUpdate = 0;

    /* PID Controller Implementation */

/*    mdl_ph2C_angleError = ((anglePowerComp + 2) / 4) - (((mdl_ph2C_leadAngleSetpoint * 3) ) );                    / * calculate angle error * / */
    mdl_ph2C_angleError = angleDiff - mdl_ph2C_leadAngleSetpoint;    /* calculate angle error */

    /* Limit measured angle and filter / remove errors */
    if (mdl_ph2C_angleError > (int16_t)((ANGLE_VARIATION) *DEGREES_TO_RADIANS))
    {
        mdl_ph2C_angleError = (ANGLE_VARIATION) *DEGREES_TO_RADIANS;
    }

    if (mdl_ph2C_angleError < (int16_t)((-(ANGLE_VARIATION)) * DEGREES_TO_RADIANS))
    {
        mdl_ph2C_angleError = (-(ANGLE_VARIATION)) * DEGREES_TO_RADIANS;
    }

    /* Integration of I term; sum of all angle errors */
    mdl_ph2C_angleErrorSum = mdl_ph2C_angleErrorSum + mdl_ph2C_angleError;

    /* Limit integrator min/max */
    if (mdl_ph2C_angleErrorSum > (int32_t)ANGLE_PID_I_LIMIT)
    {
        mdl_ph2C_angleErrorSum = (int32_t)ANGLE_PID_I_LIMIT;
    }
    else
    {
        if (mdl_ph2C_angleErrorSum < 0)
        {
            mdl_ph2C_angleErrorSum = 0;     /* Limitation of I term */
        }
        else
        {
            ; /* to pass MISRA for if-elseif-else */
        }
    }

    /* Differentiation for D term */
    mdl_ph2C_angleErrorDiff = mdl_ph2C_angleError - mdl_ph2C_angleErrorKeep;
    mdl_ph2C_angleErrorKeep = mdl_ph2C_angleError;

    /* Limit integrator min/max */
    if (mdl_ph2C_angleErrorDiff > ANGLE_PID_D_LIMIT)
    {
        mdl_ph2C_angleErrorDiff = ANGLE_PID_D_LIMIT;
    }
    else
    {
        if (mdl_ph2C_angleErrorDiff < -ANGLE_PID_D_LIMIT)
        {
            mdl_ph2C_angleErrorDiff = -ANGLE_PID_D_LIMIT;     /* Limitation of I term */
        }
        else
        {
            ; /* to pass MISRA for if-elseif-else */
        }
    }

    controllerY = (int32_t)((int32_t)mulI32_I16byI16(mdl_ph2C_angleError, Kp_LA) +
                            (int32_t)mulI32lo_I32byI16(mdl_ph2C_angleErrorSum, Ki_LA) +
                            (int32_t)mulI32_I16byI16(mdl_ph2C_angleErrorDiff, Kd_LA)); /* PI Controller Equation */


    speedUpdate = divU16_U32byU16((controllerY + (ANGLE_PID_DIVIDER / 2)), ANGLE_PID_DIVIDER);          /* rescale result for uplifted parameters Kp, KiMULTa */

    if (speedUpdate > (int16_t)(MOTOR_ROT_SPEED_MAX_RPM + 1000u))
    {
        speedUpdate = (int16_t)(MOTOR_ROT_SPEED_MAX_RPM + 1000u);
        mdl_Ph2C_MotorStop(C_WRN_PH2C_NO_SYNCHRONISATION);
    }
    else
    {
        if (speedUpdate < (int16_t)MOTOR_ROT_SPEED_ABSOLUTE_MIN_RPM)
        {
            speedUpdate = MOTOR_ROT_SPEED_ABSOLUTE_MIN_RPM;
        }
    }

    mdl_rsp_SetRotationalSpeedActualInRPM((uint16_t)speedUpdate);
}

/**
 * ----------------------------------------------------------------------------------------------
 * Phase 2C - Calculate Current Angle
 *
 * @param  - None
 * @return - no return parameter
 */
STATIC INLINE int16_t mdl_ph2C_calculateIAngle(int16_t currentU, int16_t currentV, int16_t currentW)
{
    int16_t currentAngle;
    /* Clarke Transformation of reconstructed Currents
     * Input: currentUtmp, currentVtmp, currentWtmp
     * output: IAngle [-PI .. PI] rad
     *
     * Step1:     Calculation of new Angle
     */
    fract15 Ialpha;
    fract15 Ibeta;

    /* Inverse Clarke transformation  bc=3.3us / wc=22us*/
    fract15 term1 = currentV;
    fract15 term2 = currentW;

    Ialpha = (fract15)(currentU);
    Ibeta  = mulQ15_Q15byQ15(CONVERT_TO_FRACT15(SQRT3_INV), (fract15)((term1 - term2)));

    /* Calculate current angle */
    currentAngle = (atan2I16(Ibeta, Ialpha));

    return currentAngle;
}

/**
 * ----------------------------------------------------------------------------------------------
 * Phase 2C - Calculate Voltage Angle
 *
 * @param  - None
 * @return - no return parameter
 */
STATIC INLINE int16_t mdl_ph2C_calculateVAngle(uint16_t sineIndex)
{
    int16_t mdl_ph2C_VAngle;
    uint16_t calcIndex = sineIndex + DEG60_RAD;

    /* Calculate voltage angle from table index */

    /* Calculate fixed point Q value of voltage angle */
    /* */
    /*    192 steps: */
    /*    voltageAngle = calcIndex * 341.33; */
    /*    voltageAngle = calcIndex * 21845 / 64; */
    /* voltageAngle = (int16_t)((uint32_t)mulI32_I16byI16(calcIndex, 21845) >> 6); */
    /* */
    /*    384 steps: */
    /*    voltageAngle = calcIndex * 170.66; */
    /*    voltageAngle = calcIndex * 21845 / 128; */
    mdl_ph2C_VAngle = (int16_t)calcIndex;   /* from 0 -> 2*rad to -rad -> rad */

    return mdl_ph2C_VAngle;
}

/**
 * ----------------------------------------------------------------------------------------------
 * Phase 2C - Calculate Angle Difference
 *
 * @param  - None
 * @return - no return parameter
 */
STATIC INLINE int16_t mdl_ph2C_calculateLeadAngle(int16_t currentAngle, int16_t voltageAngle)
{
    int16_t angleDiff;

    angleDiff = voltageAngle - currentAngle;

    return angleDiff;
}


/**
 * ----------------------------------------------------------------------------------------------
 * Phase 2C - angle controller update
 *
 * @param - no parameters
 * @return - no return parameter
 */

uint16_t iAngleKeep;
uint16_t cdiAngleKeep;

void mdl_ph2C_angleControllerUpdate(void)
{
    int16_t mdl_ph2C_IAngle;
    int16_t mdl_ph2C_VAngle;

    if (currentReconstructValid == 1u)
    {
        currentReconstructValid = 0u;

        /* IV angle processing */
        mdl_ph2C_IAngle         = mdl_ph2C_calculateIAngle(currentUvalid, currentVvalid, currentWvalid);
        mdl_ph2C_VAngle         = mdl_ph2C_calculateVAngle(sineIndexValid);

        mdl_ph2C_leadAngle      = mdl_ph2C_calculateLeadAngle(mdl_ph2C_IAngle, mdl_ph2C_VAngle);

        /*
         * Lead Angle Control enabled
         */
        mdl_ph2C_leadAngleController(mdl_ph2C_leadAngle);
    }
}

/* ==========================================================================
 * Implementation private functions
 * ========================================================================== */


