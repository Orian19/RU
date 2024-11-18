using System;

namespace Ex03.GarageLogic
{
    public class Truck : Vehicle
    {
        private bool m_CarryDangerousMaterials;
        private float m_CargoVolume;
        private const float k_MaxAirPressure = 28f;

        public Truck(int i_NumOfWheels, string i_ModelName, string i_LicenseNumber, EnergySource i_Engine, bool i_CarryDangerousMaterials, float i_CargoVolume)
            : base(i_NumOfWheels, i_ModelName, i_LicenseNumber, i_Engine)
        {
            m_CarryDangerousMaterials = i_CarryDangerousMaterials;
            m_CargoVolume = i_CargoVolume;
        }

        public bool HazardousMaterials
        {
            get { return m_CarryDangerousMaterials; }
            set { m_CarryDangerousMaterials = value; }
        }

        public float CargoVolume
        {
            get { return m_CargoVolume; }
            set
            {
                if (value >= 0)
                {
                    m_CargoVolume = value;
                }
                else
                {
                    throw new ArgumentException("Invalid Cargo volume input");
                }
            }
        }

        public override string ToString()
        {
            string hazardousMsg = HazardousMaterials ? "does" : "does not";

            return $@"{base.ToString()}
The truck {hazardousMsg} contain hazardous materials, and its cargo capacity is {CargoVolume}.";
        }

    }
}
