module WTP_Types
   
   use NWTC_Library
   type, public ::  WTP_Case
      real(ReKi)      :: WndSpeed
      real(ReKi)      :: TSR
      real(ReKi)      :: RotSpeed
      real(ReKi)      :: Pitch
      real(ReKi)      :: Yaw
   end type WTP_Case
   
   
   type, public ::  WTP_InputFileData
      logical         :: echo   
      character(1024) :: runTitle
      integer         :: numSect
      character(1024) :: AD_InputFile
      real(ReKi)      :: SWTol
      !real(ReKi)      :: AIDragM
      !real(ReKi)      :: TIDragM
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
      real(ReKi),allocatable :: Twist(:,:)
      real(ReKi),allocatable :: rLoc(:,:)
      
      real(ReKi)      :: ShearExp
      character(1024) :: OutFileRoot
      logical         :: UnfPower
      logical         :: TabDel
      logical         :: OutNines
      logical         :: KFact
      logical         :: InputTSR
      logical         :: OutMaxCp
      logical         :: WriteBED
      integer         :: NumCases
      type(WTP_Case), allocatable  :: Cases(:)
      
   end type WTP_InputFileData
   
   type, public ::  WTP_InitInputType
      type(WTP_InputFileData) :: inpFileData
      character(1024)         :: inputFileName
      real(ReKi)              :: gravity
   end type WTP_InitInputType
   
end module WTP_Types