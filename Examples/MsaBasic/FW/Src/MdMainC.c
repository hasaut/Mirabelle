static volatile unsigned char FActive;

void WDResetRV ( void );

void MainC ( void )
{
 FActive=1;
 do
  {
   WDResetRV();
  } while(FActive);
}

