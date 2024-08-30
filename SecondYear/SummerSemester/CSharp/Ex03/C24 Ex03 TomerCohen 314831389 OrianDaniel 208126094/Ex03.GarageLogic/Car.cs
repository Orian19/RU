using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex03.GarageLogic
{
    public class Car : Vehicle
    {
        private readonly eColors r_Color;
        private readonly eDoors r_Doors;
        public Car(int i_NumOfWheels, string i_ModelName, string i_LicenseNumber, Engine i_Engine, eLicenceType i_LicenseType, int i_EngineVolume)
            : base(i_NumOfWheels, i_LicenseNumber, i_ModelName, i_Engine)
        {
        }

        protected override float RemainingEnergyPercentage()
        {
            throw new NotImplementedException();
        }
    }
}
