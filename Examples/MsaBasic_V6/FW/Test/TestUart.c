volatile unsigned char FRecvLen; // Set to non-zero if packet received. Set to zero when packet is processed
unsigned char FRecvBuf[256];     // If a new packet arrives while previous is not processed, continue reception (not to desynchronize) but do not store data

unsigned short UartGetData ( void ); // >255 if error

unsigned char FUartActive;

void UartRecvA ( void )
{
 unsigned short BUartData;
 unsigned char BLen, BData, BSum;
 unsigned char BIndex;
 unsigned char BSaveData; // Prevent reception if previous data was not yet read. Keep trace but do not save

 do
  {
   do
    {
     // Header
     BUartData=UartGetData(); if (BUartData!=0x55) break;
     if (FRecvLen==0) BSaveData=1; else BSaveData=0;
     // Length
     BUartData=UartGetData(); if (BUartData>255) break;
     BLen=BUartData;
     // Data
     BSum=BLen;
     for (BIndex=0; BIndex<BLen; BIndex++)
      {
       BUartData=UartGetData(); if (BUartData>255) break;
       BData=BUartData; // Typecast
       BSum+=BData;
       if (BSaveData) FRecvBuf[BIndex]=BData;
      }
     if (BIndex<BLen) break;
     // Checksum
     BUartData=UartGetData(); if (BUartData>255) break;
     if (BSum!=BUartData) break;
     if (BSaveData==0) break;
     FRecvLen=BLen;
    } while(0);
   /*if (BUartData>255)
    {
     // Analyse error
    }*/
  } while(FUartActive); // Need this for avoid "unreachable code" message
}


#define CStHdr  0
#define CStLen  1
#define CStData 2
#define CStSum  3

volatile unsigned char FState;
volatile unsigned char FWorkLen, FWorkIdx, FWorkSum;
volatile unsigned char FWorkSaveData;

void UartRecvB ( void )
{
 unsigned short BUartData;
 unsigned char BLen, BIdx;
 unsigned char BData;

 do
  {
   BUartData=UartGetData();
   if (BUartData>255) { FState=CStHdr; break; }

   switch (FState)
    {
     case CStHdr:
       if (BUartData!=0x55) break;
       FState=CStLen;
       break;

     case CStLen:
       BLen=BUartData;
       FWorkLen=BLen;
       if (FRecvLen==0) FWorkSaveData=1; else FWorkSaveData=0;
       FWorkSum=BLen;
       FWorkIdx=0;
       if (BLen==0) FState=CStSum; else FState=CStData;
       break;

     case CStData:
       BData=BUartData; // Typecast
       BIdx=FWorkIdx; // it generates somewhat smaller code
       if (FWorkSaveData) FRecvBuf[BIdx]=BData;
       BIdx++;
       FWorkSum+=BData;
       FWorkIdx=BIdx;
       if (BIdx>=FWorkLen) FState=CStSum;
       break;

     case CStSum:
       FState=CStHdr;
       if (FWorkSaveData==0) break;
       BData=BUartData;
       if (BData!=FWorkSum) break;
       FRecvLen=FWorkLen;
       break;

     default:
       FState=CStHdr;
    }
  } while(0);
}


