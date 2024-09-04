using System;

namespace Ex03.GarageLogic
{
    public abstract class EnergySource
    {
        protected readonly float r_MaxEnergyCapacity;
        protected float m_RemainingEnergyPercentage;

        public EnergySource(float i_MaxEnergyCapcity)
        {
            r_MaxEnergyCapacity = i_MaxEnergyCapcity;
        }

        public float MaxEnergyCapacity
        {
            get { return r_MaxEnergyCapacity; }
        }

        public float RemainingEnergyPercentage
        {
            get { return m_RemainingEnergyPercentage; }
            set
            {
                if (value >= 0 && value <= 100)
                {
                    m_RemainingEnergyPercentage = value;
                }
                else
                {
                    Exception ex = new Exception("Invalid input for the percentage of remaining energy.");
                    throw new ValueOutOfRangeException(0f, 100f, ex);
                }
            }
        }
    }
}
