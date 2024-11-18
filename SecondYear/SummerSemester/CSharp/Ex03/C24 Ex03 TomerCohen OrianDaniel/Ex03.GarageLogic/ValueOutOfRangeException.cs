using System;

namespace Ex03.GarageLogic
{
    public class ValueOutOfRangeException : Exception
    {
        private readonly float r_MinValue;
        private readonly float r_MaxValuer;

        public ValueOutOfRangeException(float i_MinValue, float i_MaxValue, Exception i_InnerException)
            : base($"Value out of range. Should be between {i_MinValue} and {i_MaxValue}.", i_InnerException)
        {
            r_MinValue = i_MinValue;
            r_MaxValuer = i_MaxValue;
        }

        public float MinValue
        {
            get { return r_MinValue; }
        }

        public float MaxValue
        {
            get { return r_MaxValuer; }
        }
    }
}
