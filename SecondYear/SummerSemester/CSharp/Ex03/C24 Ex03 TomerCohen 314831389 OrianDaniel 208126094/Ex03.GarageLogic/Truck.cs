using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace Ex03.GarageLogic
{
    public class Truck : Vehicle
    {
        private bool m_CarryDangerousMaterials;
        private float m_CargoVolume;

        public Truck(int i_NumOfWheels, string i_ModelName, string i_LicenseNumber, EnergySource i_Engine, bool i_CarryDangerousMaterials, float i_CargoVolume)
            : base(i_NumOfWheels, i_ModelName, i_LicenseNumber, i_Engine)
        {
            m_CarryDangerousMaterials = i_CarryDangerousMaterials;
            m_CargoVolume = i_CargoVolume;
        }
        
        protected override float RemainingEnergyPercentage()
        {
            throw new NotImplementedException();
        }
    }
}
