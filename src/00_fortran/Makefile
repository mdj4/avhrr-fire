FC = gfortran

INCLUDE_NETCDF=/group_workspaces/cems2/esacci_sst/software/common.gfortran/include
LIB_NETCDF=-L/group_workspaces/cems2/esacci_sst/software/common.gfortran/lib -lnetcdff -lnetcdf 

#INCLUDE = -g -fbounds-check -cpp -DUSE_NETCDF -I. -I/group_workspaces/cems2/esacci_sst/software/common.gfortran/include
INCLUDE = -O -cpp -DUSE_NETCDF -I. -I$(INCLUDE_NETCDF)
LIBRARY = $(LIB_NETCDF)

OBJECTS_GBCS = GbcsPath.o GbcsKinds.o GbcsConstants.o GbcsBaseTypes.o GbcsDateTime.o GbcsPixel.o GbcsErrorHandler.o GbcsPDFLookup.o GbcsTypes.o GbcsSystemTools.o GbcsTimeSpace.o GbcsInterpolators.o GbcsScattPhysics.o GbcsForecastModel.o GbcsMatrixOps.o GbcsPDFLoaders.o GbcsPixelLoaders.o GbcsStringUtil.o NOAA_LoadAVHRRLevel1B.o GbcsAtmPhysics.o GbcsGeopotential.o GbcsProfileGenerator.o GbcsCleanUp.o Util.o
OBJECTS_extract = extract_l1b_data.o
OBJECTS = $(OBJECTS_GBCS) $(OBJECTS_extract)

all: $(OBJECTS)
	$(FC) -o extract_l1b_data.exe $(OBJECTS_GBCS) $(OBJECTS_extract) $(LIBRARY)

GbcsPath.o: GbcsPath.f90
	$(FC) $(INCLUDE) -c GbcsPath.f90
GbcsKinds.o: GbcsKinds.f90
	$(FC) $(INCLUDE) -c GbcsKinds.f90
GbcsConstants.o: GbcsKinds.f90 GbcsConstants.f90
	$(FC) $(INCLUDE) -c GbcsConstants.f90
GbcsBaseTypes.o: GbcsKinds.f90 GbcsBaseTypes.f90
	$(FC) $(INCLUDE) -c GbcsBaseTypes.f90
GbcsDateTime.o: GbcsKinds.f90 GbcsDateTime.f90
	$(FC) $(INCLUDE) -c GbcsDateTime.f90
GbcsPixel.o: GbcsKinds.f90 GbcsDateTime.f90 GbcsPixel.f90
	$(FC) $(INCLUDE) -c GbcsPixel.f90
GbcsErrorHandler.o: GbcsKinds.f90 GbcsErrorHandler.f90
	$(FC) $(INCLUDE) -c GbcsErrorHandler.f90
GbcsPDFLookup.o: GbcsKinds.f90 GbcsPixel.f90 GbcsPDFLookup.f90
	$(FC) $(INCLUDE) -c GbcsPDFLookup.f90
GbcsTypes.o: GbcsKinds.f90 GbcsBaseTypes.f90 GbcsTypes.f90
	$(FC) $(INCLUDE) -c GbcsTypes.f90
GbcsSystemTools.o: GbcsPath.f90 GbcsKinds.f90 GbcsTypes.f90 GbcsSystemTools.f90
	$(FC) $(INCLUDE) -c GbcsSystemTools.f90
GbcsTimeSpace.o: GbcsKinds.f90 GbcsTypes.f90 GbcsDateTime.f90 GbcsTimeSpace.f90
	$(FC) $(INCLUDE) -c GbcsTimeSpace.f90
GbcsInterpolators.o: GbcsKinds.f90 GbcsInterpolators.f90
	$(FC) $(INCLUDE) -c GbcsInterpolators.f90
GbcsScattPhysics.o: GbcsKinds.f90 GbcsTypes.f90 GbcsConstants.f90 GbcsInterpolators.f90 GbcsScattPhysics.f90
	$(FC) $(INCLUDE) -c GbcsScattPhysics.f90
GbcsForecastModel.o: GbcsTypes.f90 GbcsErrorHandler.f90 GbcsForecastModel.f90
	$(FC) $(INCLUDE) -c GbcsForecastModel.f90
GbcsMatrixOps.o: GbcsKinds.f90 GbcsMatrixOps.f90
	$(FC) $(INCLUDE) -c GbcsMatrixOps.f90
GbcsPDFLoaders.o: GbcsTypes.f90 GbcsErrorHandler.f90 GbcsConstants.f90 GbcsPDFLoaders.f90
	$(FC) $(INCLUDE) -c GbcsPDFLoaders.f90
GbcsPixelLoaders.o: GbcsKinds.f90 GbcsTypes.f90 GbcsErrorHandler.f90 GbcsPixelLoaders.f90
	$(FC) $(INCLUDE) -c GbcsPixelLoaders.f90
NOAA_LoadAVHRRLevel1B.o: GbcsKinds.f90 GbcsTypes.f90 GbcsConstants.f90 NOAA_LoadAVHRRLevel1B.f90
	$(FC) $(INCLUDE) -c NOAA_LoadAVHRRLevel1B.f90
GbcsAtmPhysics.o: GbcsKinds.f90 GbcsTypes.f90 GbcsAtmPhysics.f90
	$(FC) $(INCLUDE) -c GbcsAtmPhysics.f90
GbcsGeopotential.o: GbcsKinds.f90 GbcsTypes.f90 GbcsAtmPhysics.f90 GbcsGeopotential.f90
	$(FC) $(INCLUDE) -c GbcsGeopotential.f90
GbcsProfileGenerator.o: GbcsKinds.f90 GbcsTypes.f90 GbcsGeopotential.f90 GbcsProfileGenerator.f90
	$(FC) $(INCLUDE) -c GbcsProfileGenerator.f90
GbcsCleanUp.o: GbcsErrorHandler.f90 GbcsCleanUp.f90
	$(FC) $(INCLUDE) -c GbcsCleanUp.f90
GbcsStringUtil.o: GbcsStringUtil.f90
	$(FC) $(INCLUDE) -c GbcsStringUtil.f90
Util.o: GbcsKinds.f90 GbcsTypes.f90 GbcsErrorHandler.o
	$(FC) $(INCLUDE) -c Util.f90
extract_l1b_data.o: GbcsKinds.f90 GbcsTypes.f90 GbcsConstants.f90 GbcsErrorHandler.f90 GbcsDateTime.f90 NOAA_LoadAVHRRLevel1B.f90 extract_l1b_data.f90
	$(FC) $(INCLUDE) -c extract_l1b_data.f90

.PHONY:clean
clean:
	rm -rf *.mod *.o *.exe

