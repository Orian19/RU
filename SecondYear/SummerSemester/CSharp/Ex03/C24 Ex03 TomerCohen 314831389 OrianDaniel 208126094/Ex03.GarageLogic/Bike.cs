using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex03.GarageLogic
{
    public class Bike : Vehicle
    {
        private readonly eLicenseType r_LicenseType;
        private readonly int r_EngineVolume;

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

        protected override float RemainingEnergyPercentage()
        {
            throw new NotImplementedException();
        }
    }
}
