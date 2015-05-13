module WTP_Types
   
   use NWTC_Library
   type, public ::  WTP_Case
      real(ReKi)      :: WndSpeed
      real(ReKi)      :: TSR
      real(ReKi)      :: RotSpeed
      real(ReKi)      :: Pitch
      real(ReKi)      :: Yaw
   end type WTP_Case
   
   type, public ::  WTP_Blade
      integer         :: AFfile
      real(ReKi)      :: RLoc
      real(ReKi)      :: Twist
      real(ReKi)      :: Chord
      logical         :: PrntElem
      real(ReKi)      :: RLocND
   end type WTP_Blade
   
   type, public ::  WTP_InputFileData
      logical         :: echo   
      character(1024) :: runTitle
      logical         :: dimenInp
      integer         :: numSect
      character(1024) :: AD_InputFile
      integer         :: maxIter
      real(ReKi)      :: ATol
      real(ReKi)      :: ATol2
      real(ReKi)      :: SWTol
      logical         :: useTipLoss           ! BEM input
      logical         :: useHubLoss           ! BEM input
      logical         :: useTanInd             ! BEM input useTanInd
      integer         :: skewWakeMod
      logical         :: useInduction
      logical         :: useAIDrag            ! BEM input
      logical         :: useTIDrag            ! BEM input
      real(ReKi)      :: AIDragM
      real(ReKi)      :: TIDragM
      logical         :: TISingularity
      integer         :: numBlade
      real(ReKi)      :: rotorRad
      real(ReKi)      :: hubRad
      real(ReKi)      :: preCone
      real(ReKi)      :: tilt
      real(ReKi)      :: yaw
      real(ReKi)      :: hubHt
      integer         :: numSeg
      real(ReKi)      :: HubRadND
      real(ReKi)      :: HubHtND
      real(ReKi)      :: BldLen
      real(ReKi)      :: SinCone
      real(ReKi)      :: CosCone
      real(ReKi)      :: CosTilt
      real(ReKi)      :: SinTilt
      real(ReKi)      :: CosYaw
      real(ReKi)      :: SinYaw
      logical         :: DoSkew
      real(ReKi)      :: SWconst
      logical         :: SWconv
      real(ReKi)      :: SWcorr
      type(WTP_Blade), allocatable :: BladeData(:)
      real(ReKi)      :: AirDens
      real(ReKi)      :: KinVisc
      real(ReKi)      :: ShearExp
      character(1024) :: OutFileRoot
      logical         :: UnfPower
      logical         :: TabDel
      logical         :: OutNines
      logical         :: Beep
      logical         :: KFact
      logical         :: InputTSR
      logical         :: OutMaxCp
      logical         :: WriteBED
      character(10)   :: SpdUnits
      integer         :: NumCases
      type(WTP_Case), allocatable  :: Cases(:)
      
   end type WTP_InputFileData
   
   type, public ::  WTP_InitInputType
      type(WTP_InputFileData) :: inpFileData
      character(1024)         :: inputFileName
      real(ReKi)              :: gravity
   end type WTP_InitInputType
   
end module WTP_Types