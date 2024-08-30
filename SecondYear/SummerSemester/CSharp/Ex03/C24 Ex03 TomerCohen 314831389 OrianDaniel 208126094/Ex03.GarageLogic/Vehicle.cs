using System.Collections.Generic;

namespace Ex03.GarageLogic
{
    public abstract class Vehicle
    {
        protected readonly string r_ModelName;
        protected readonly string r_LicenseNumber;
        protected readonly EnergySource r_Engine;
        protected float m_RemainingEnergyPercentage;
        protected List<Wheel> m_Wheels;

        protected Vehicle(int i_NumOfWheels, string i_ModelName, string i_LicenseNumber, EnergySource i_Engine)
        {
            r_LicenseNumber = i_LicenseNumber;
            r_ModelName = i_ModelName; 
            r_Engine = i_Engine;
            m_Wheels = new List<Wheel>(i_NumOfWheels);
        }

        public string ModelName
        {
            get { return r_ModelName; }
        }

        public string LicenseNumber
        {
            get { return r_LicenseNumber; }
        }

        public List<Wheel> Wheels
        {
            get { return m_Wheels; }
            set { m_Wheels = value; }
        }

        protected abstract float RemainingEnergyPercentage();

    }
}
