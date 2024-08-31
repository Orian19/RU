using System;
using System.Collections.Generic;
using System.Reflection;
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

        public Vehicle CreateVehicle(eVehiclesTypes i_VehicleTypes, string i_LicenseNumber, Dictionary<eOptions, object> i_Options, string i_ModelName)
        {
            Vehicle newVehicle = null;

            switch (i_VehicleTypes)
            {
                case eVehiclesTypes.RegularBike:
                    {
                        if (i_Options.TryGetValue(eOptions.CurrentFuel, out object CurrentFuelObj) && CurrentFuelObj is float currentFuel)
                        {
                            Fuel energy = new Fuel(6, eFuelType.Octan98, currentFuel);

                            if (i_Options.TryGetValue(eOptions.LicenseType, out object licenseTypeObj) && licenseTypeObj is eLicenseType licenseType &&
                                i_Options.TryGetValue(eOptions.EngineVolume, out object engineVolObj) && licenseTypeObj is int engingeVol)
                            {
                                newVehicle = new Bike(2, i_ModelName, i_LicenseNumber, energy, licenseType, engingeVol);
                            }
                        }

                        break;
                    }

                case eVehiclesTypes.ElectricBike:
                    {
                        if (i_Options.TryGetValue(eOptions.BatteryTimeRemaining, out object batteryTimeRemainingObj) && batteryTimeRemainingObj is float batteryTimeRemaining)
                        {
                            Electricity energy = new Electricity(2.7f, batteryTimeRemaining);

                            if (i_Options.TryGetValue(eOptions.LicenseType, out object licenseTypeObj) && licenseTypeObj is eLicenseType licenseType &&
                                i_Options.TryGetValue(eOptions.EngineVolume, out object engineVolObj) && licenseTypeObj is int engingeVol)
                            {
                                newVehicle = new Bike(2, i_ModelName, i_LicenseNumber, energy, licenseType, engingeVol);
                            }
                        }                  

                        break;
                    }

                case eVehiclesTypes.RegularCar:
                    {
                        if (i_Options.TryGetValue(eOptions.CurrentFuel, out object CurrentFuelObj) && CurrentFuelObj is float currentFuel)
                        {
                            Fuel energy = new Fuel(49, eFuelType.Octan95, currentFuel);

                            if (i_Options.TryGetValue(eOptions.Color, out object colorObj) && colorObj is eColors color &&
                                i_Options.TryGetValue(eOptions.Doors, out object doorsObj) && doorsObj is eDoors doors)
                            {
                                newVehicle = new Car(5, i_ModelName, i_LicenseNumber, energy,  color, doors);
                            }
                        }

                        break;
                    }

                case eVehiclesTypes.ElectricCar:
                    {
                        if (i_Options.TryGetValue(eOptions.BatteryTimeRemaining, out object batteryTimeRemainingObj) && batteryTimeRemainingObj is float batteryTimeRemaining)
                        {
                            Electricity energy = new Electricity(5f, batteryTimeRemaining);

                            if (i_Options.TryGetValue(eOptions.Color, out object colorObj) && colorObj is eColors color &&
                                i_Options.TryGetValue(eOptions.Doors, out object doorsObj) && doorsObj is eDoors doors)
                            {
                                newVehicle = new Car(5, i_ModelName, i_LicenseNumber, energy, color, doors);
                            }
                        }

                        break;
                    }

                case eVehiclesTypes.Truck:
                    {
                        if (i_Options.TryGetValue(eOptions.CurrentFuel, out object CurrentFuelObj) && CurrentFuelObj is float currentFuel)
                        {
                            Fuel energy = new Fuel(130, eFuelType.Soler, currentFuel);

                            if (i_Options.TryGetValue(eOptions.CarryDangerousMaterialsDang, out object carryDangerousMaterialsObj) && carryDangerousMaterialsObj is bool carryDangerousMaterials &&
                                i_Options.TryGetValue(eOptions.CargoVolume, out object cargoVolumeObj) && cargoVolumeObj is float cargoVolume)
                            {
                                newVehicle = new Truck(14, i_ModelName, i_LicenseNumber, energy, carryDangerousMaterials, cargoVolume);
                            }
                        }

                        break;
                    }
            }

            return newVehicle;
        }

        public Dictionary<eOptions, object> GetOptions(eVehiclesTypes i_VehicleType)
        {
            Dictionary<eOptions, object> options = new Dictionary<eOptions, object>();

            switch (i_VehicleType)
            {
                case eVehiclesTypes.RegularBike:
                    addBikeOptions(options);
                    addFuelOptions(options);
                    break;

                case eVehiclesTypes.ElectricBike:
                    addBikeOptions(options);
                    addElectricityOptions(options);
                    break;

                case eVehiclesTypes.RegularCar:
                    addCarOptions(options);
                    addFuelOptions(options);
                    break;

                case eVehiclesTypes.ElectricCar:
                    addCarOptions(options);
                    addElectricityOptions(options);
                    break;

                case eVehiclesTypes.Truck:
                    options.Add(eOptions.CarryDangerousMaterialsDang, typeof(bool));
                    options.Add(eOptions.CargoVolume, typeof(float));
                    addFuelOptions(options);
                    break;
            }

            return options;
        }

        private void addBikeOptions(Dictionary<eOptions, object> i_Options)
        {
            i_Options.Add(eOptions.LicenseType, typeof(eLicenseType));
            i_Options.Add(eOptions.EngineVolume, typeof(int));
        }

        private void addCarOptions(Dictionary<eOptions, object> i_Options)
        {
            i_Options.Add(eOptions.Color, typeof(eColors));
            i_Options.Add(eOptions.Doors, typeof(eDoors));
        }

        private void addFuelOptions(Dictionary<eOptions, object> i_Options)
        {
            i_Options.Add(eOptions.FuelType, typeof(eFuelType));
            i_Options.Add(eOptions.CurrentFuel, typeof(float));
            i_Options.Add(eOptions.MaxFuel, typeof(float));
        }

        private void addElectricityOptions(Dictionary<eOptions, object> i_Options)
        {
            i_Options.Add(eOptions.BatteryTimeRemaining, typeof(float));
            i_Options.Add(eOptions.MaxBatteryTime, typeof(float));
        }

        public string DisplayListOfLicenseNumbers(bool i_InRepair, bool i_Repaired, bool i_Paid)
        {
            StringBuilder licensesList = new StringBuilder();

            foreach (KeyValuePair<string, OwnerInfo> ownerEntry in r_Vehicles)
            {
                if (i_InRepair && ownerEntry.Value.VehicleState.Equals(eVehicleState.InRepair))
                {
                    licensesList.AppendLine($"{ownerEntry.Key}");
                }

                if (i_Repaired && ownerEntry.Value.VehicleState.Equals(eVehicleState.Repaired))
                {
                    licensesList.AppendLine($"{ownerEntry.Key}");
                }

                if (i_Paid && ownerEntry.Value.VehicleState.Equals(eVehicleState.Paid))
                {
                    licensesList.AppendLine($"{ownerEntry.Key}");
                }
            }

            if (licensesList.Length == 0)
            {
                licensesList.Append(k_LicenseErrorMsg);
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
                    fueledVehicle.CurrentFuel += i_FuelAmount;
                    ownerInfo.Vehicle.RemainingEnergyPercentage = (fueledVehicle.CurrentFuel / fueledVehicle.MaxEnergyCapacity) * 100;
                }
                else
                {
                    throw new ArgumentException("Vehicle energy is fuel");
                }

            }
            else
            {
                throw new ArgumentException(k_LicenseErrorMsg);
            }
        }

        public void RechargeVehicle(string  i_LicenseNumber, float i_minutesToCharge)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                if (ownerInfo.Vehicle.EnegrySource is Electricity electricVehicle)
                {
                    electricVehicle.Recharge(i_minutesToCharge);
                    electricVehicle.BatteryTimeRemaining += i_minutesToCharge;
                    ownerInfo.Vehicle.RemainingEnergyPercentage = (electricVehicle.BatteryTimeRemaining / electricVehicle.MaxEnergyCapacity) * 100;
                }
                else
                {
                    throw new ArgumentException("Vehicle energy is electricity");
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
