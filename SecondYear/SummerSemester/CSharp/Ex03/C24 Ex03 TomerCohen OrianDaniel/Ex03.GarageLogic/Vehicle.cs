﻿using System;
using System.Collections.Generic;
using System.Text;

namespace Ex03.GarageLogic
{
    public abstract class Vehicle
    {
        protected readonly string r_ModelName;
        protected readonly string r_LicenseNumber;
        protected readonly EnergySource r_EnergySource;
        protected List<Wheel> m_Wheels;

        protected Vehicle(int i_NumOfWheels, string i_ModelName, string i_LicenseNumber, EnergySource i_Energy)
        {
            r_LicenseNumber = i_LicenseNumber;
            r_ModelName = i_ModelName;
            r_EnergySource = i_Energy;
            m_Wheels = new List<Wheel>(i_NumOfWheels);
        }

        public EnergySource EnegrySource
        {
            get { return r_EnergySource; }
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

        public override string ToString()
        {
            StringBuilder vehicle = new StringBuilder();
            int wheelIndex = 1;

            vehicle.Append($@"License Number is: {r_LicenseNumber}
Model Name is: {r_ModelName}
Wheels info:
");

            foreach (Wheel wheel in Wheels)
            {
                vehicle.AppendLine($"Wheel {wheelIndex++}: {wheel.ToString()}");
            }

            vehicle.AppendLine(r_EnergySource.ToString());

            return vehicle.ToString();
        }

    }
}
