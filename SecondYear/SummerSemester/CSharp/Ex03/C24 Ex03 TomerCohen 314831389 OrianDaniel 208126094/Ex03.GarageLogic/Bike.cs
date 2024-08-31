﻿namespace Ex03.GarageLogic
{
    public class Bike : Vehicle
    {
        private readonly eLicenseType r_LicenseType;
        private readonly int r_EngineVolume;
        private const float k_MaxAirPressure = 31f;

        public Bike(int i_NumOfWheels, string i_ModelName, string i_LicenseNumber, EnergySource i_Engine, eLicenseType i_LicenseType, int i_EngineVolume) 
            : base(i_NumOfWheels, i_LicenseNumber, i_ModelName, i_Engine)
        {
            r_LicenseType = i_LicenseType;
            r_EngineVolume = i_EngineVolume;
        }

        public eLicenseType LicenceType
        {
            get {  return r_LicenseType; }
        }

        public int EngineVolume
        {
            get { return r_EngineVolume; } 
        }

        public override string ToString()
        {
            return $@"
{base.ToString()} 
License Type: {r_LicenseType}
Engine Volume: {r_EngineVolume}
";
        }
    }
}
