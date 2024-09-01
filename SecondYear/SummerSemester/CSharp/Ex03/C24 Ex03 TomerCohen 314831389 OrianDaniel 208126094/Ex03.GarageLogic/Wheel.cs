using System;

namespace Ex03.GarageLogic
{
    public class Wheel
    {
        private readonly string r_ManufacturerName;
        private readonly float r_MaxAirPressure;
        private float m_CurrentAirPressure;

        public Wheel(string i_ManufacturerName, float i_MaxAirPressure)
        {
            r_ManufacturerName = i_ManufacturerName;
            r_MaxAirPressure = i_MaxAirPressure;
        }
        public string ManufacturerName
        {
            get { return r_ManufacturerName; }
        }

        public float CurrentAirPressure
        {
            get { return m_CurrentAirPressure; }
            set
            {
                if (value >= 0 && value <= MaxAirPressure)
                {
                    m_CurrentAirPressure = value;
                }
                else
                {
                    Exception ex = new Exception("Invalid input for wheel air pressure.");
                    throw new ValueOutOfRangeException(0f, MaxAirPressure, ex);
                }
            }
        }

        public float MaxAirPressure
        {
            get { return r_MaxAirPressure; }
        }

        public void inflate(float i_AirToAdd)
        {
            if (m_CurrentAirPressure + i_AirToAdd > r_MaxAirPressure)
            {
                Exception ex = new Exception("Invalid input for adding pressure to the wheel..");
                throw new ValueOutOfRangeException(0, r_MaxAirPressure, ex);
            }

            m_CurrentAirPressure += i_AirToAdd;
        }

        public override string ToString()
        {
            return $"Manufacturer is {ManufacturerName}, current air pressure is {CurrentAirPressure} out of {MaxAirPressure}.";
        }
    }
}