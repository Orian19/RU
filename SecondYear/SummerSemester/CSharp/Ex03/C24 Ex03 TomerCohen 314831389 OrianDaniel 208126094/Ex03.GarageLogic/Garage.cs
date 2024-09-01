using System;
using System.Collections.Generic;
using System.Text;

namespace Ex03.GarageLogic
{
    public class Garage
    {
        private readonly Dictionary<string, OwnerInfo> r_Vehicles;
        private const string k_LicenseErrorMsg = "No vehicle with this license number was found";

        public Garage()
        {
            r_Vehicles = new Dictionary<string, OwnerInfo>();
        }

        public bool AddVehicle(OwnerInfo i_OwnerInfo)
        {
            bool vehicleAdded = false;

            if (r_Vehicles.ContainsKey(i_OwnerInfo.Vehicle.LicenseNumber))
            {
                try
                {
                    i_OwnerInfo.VehicleState = eVehicleState.InRepair;
                }
                catch (Exception ex)
                {
                    throw new Exception($"License number {i_OwnerInfo.Vehicle.LicenseNumber} is not in the system.", ex);
                }
            }
            else
            {
                r_Vehicles.Add(i_OwnerInfo.Vehicle.LicenseNumber, i_OwnerInfo);
                vehicleAdded = true;
            }

            return vehicleAdded;
        }

        public string DisplayListOfLicenseNumbers(bool i_InRepair, bool i_Fixed, bool i_Paid)
        {
            StringBuilder licensesList = new StringBuilder();

            if (i_InRepair)
            {
                licensesList.AppendLine("InRepair:");
                foreach (KeyValuePair<string, OwnerInfo> ownerEntry in r_Vehicles)
                {
                    if (ownerEntry.Value.VehicleState.Equals(eVehicleState.InRepair))
                    {
                        licensesList.AppendLine($"{ownerEntry.Key}");
                    }
                }
            }

            if (i_Fixed)
            {
                licensesList.AppendLine("Repaired:");
                foreach (KeyValuePair<string, OwnerInfo> ownerEntry in r_Vehicles)
                {
                    if (ownerEntry.Value.VehicleState.Equals(eVehicleState.Repaired))
                    {
                        licensesList.AppendLine($"{ownerEntry.Key}");
                    }
                }
            }

            if (i_Paid)
            {
                licensesList.AppendLine("Paid:");
                foreach (KeyValuePair<string, OwnerInfo> ownerEntry in r_Vehicles)
                {
                    if (ownerEntry.Value.VehicleState.Equals(eVehicleState.Paid))
                    {
                        licensesList.AppendLine($"{ownerEntry.Key}");
                    }
                }
            }

            if (licensesList.Length == 0)
            {
                licensesList.Append("No vehicles in the garage.");
            }

            return licensesList.ToString();
        }


        public void ChangeVehicleState(string i_LicenseNumber, eVehicleState i_VehicleState)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                ownerInfo.VehicleState = i_VehicleState;
            }
            else
            {
                throw new ArgumentException(k_LicenseErrorMsg);
            }
        }

        public void InflateToMax(string i_LicenseNumber)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                foreach (Wheel wheel in ownerInfo.Vehicle.Wheels)
                {
                    wheel.inflate(wheel.MaxAirPressure - wheel.CurrentAirPressure);
                }
            }
            else
            {
                throw new ArgumentException(k_LicenseErrorMsg);
            }
        }

        public void RefuelVehicle(string i_LicenseNumber, eFuelType i_FuelType, float i_FuelAmount)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                if (ownerInfo.Vehicle.EnegrySource is Fuel fueledVehicle)
                {
                    fueledVehicle.Refuel(i_FuelAmount, i_FuelType);
                    ownerInfo.Vehicle.RemainingEnergyPercentage = (fueledVehicle.CurrentFuel / fueledVehicle.MaxEnergyCapacity) * 100;
                }
                else
                {
                    throw new ArgumentException("To refuel Vehicle energy must be fuel");
                }

            }
            else
            {
                throw new ArgumentException(k_LicenseErrorMsg);
            }
        }

        public void RechargeVehicle(string i_LicenseNumber, float i_minutesToCharge)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                if (ownerInfo.Vehicle.EnegrySource is Electricity electricVehicle)
                {
                    electricVehicle.Recharge(i_minutesToCharge);
                    ownerInfo.Vehicle.RemainingEnergyPercentage = (electricVehicle.BatteryTimeRemaining / electricVehicle.MaxEnergyCapacity) * 100;
                }
                else
                {
                    throw new ArgumentException("To recharge Vehicle energy must be electricity");
                }
            }
            else
            {
                throw new ArgumentException(k_LicenseErrorMsg);
            }
        }

        public void DisplayFullVehicleInfo(string i_LicenseNumber)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                Console.WriteLine(ownerInfo.ToString());
            }
            else
            {
                throw new ArgumentException(k_LicenseErrorMsg);
            }
        }
    }
}