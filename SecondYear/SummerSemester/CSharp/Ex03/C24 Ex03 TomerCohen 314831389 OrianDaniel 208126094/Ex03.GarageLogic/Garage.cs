using System;
using System.Collections.Generic;

namespace Ex03.GarageLogic
{
    public class Garage
    {
        private readonly Dictionary<string, OwnerInfo> r_Vehicles;

        public Garage()
        {
            r_Vehicles = new Dictionary<string, OwnerInfo>();
        }

        public void AddVehicle(OwnerInfo i_OwnerInfo)
        {
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
            }
        }

        public static Vehicle CreateVehicle(eVehiclesTypes i_VehicleTypes, string i_LicenseNumber, List<Wheel> i_Wheels, Dictionary<eOptions, object> i_Options, string i_ModelName)
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

        public Dictionary<eOptions, Type> GetOptions(Vehicle vehicle)
        {
            Dictionary<eOptions, Type> options = new Dictionary<eOptions, Type>();

            if (vehicle is Bike)
            {
                options.Add(eOptions.LicenseType, typeof(eLicenseType));
                options.Add(eOptions.EngineVolume, typeof(int));
                if (vehicle.Enegry is Electricity)
                {
                    addElectricityOptions(options);
                }
                else
                {
                    addFuelOptions(options);
                }
            }
            else if (vehicle is Car)
            {
                options.Add(eOptions.Color, typeof(eColors));
                options.Add(eOptions.Doors, typeof(eDoors));
                if (vehicle.Enegry is Electricity)
                {
                    addElectricityOptions(options);
                }
                else
                {
                    addFuelOptions(options);
                }
            }
            else if (vehicle is Truck)
            {
                options.Add(eOptions.CarryDangerousMaterialsDang, typeof(bool));
                options.Add(eOptions.CargoVolume, typeof(float));
                addFuelOptions(options);
            }

            return options;
        }

        private void addFuelOptions(Dictionary<eOptions, Type> i_Options)
        {
            i_Options.Add(eOptions.FuelType, typeof(eFuelType));
            i_Options.Add(eOptions.CurrentFuel, typeof(float));
            i_Options.Add(eOptions.MaxFuel, typeof(float));
        }

        private void addElectricityOptions(Dictionary<eOptions, Type> i_Options)
        {
            i_Options.Add(eOptions.BatteryTimeRemaining, typeof(float));
            i_Options.Add(eOptions.MaxBatteryTime, typeof(float));
        }

        public void DisplayListOfLicesnseNumbers()
        {
            return;
        }

        public void ChangeVehicleState(string i_LicenseNumber, eVehicleState i_VehicleState)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
               ownerInfo.VehicleState = i_VehicleState;
            }
            else
            {
                throw new ArgumentException("No vehicle with this license number was found");
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
                throw new ArgumentException("No vehicle with this license number was found");
            }
        }

        public void RefuelVehicle(string i_LicenseNumber, eFuelType i_FuelType, float i_FuelAmount)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                if ()
            }
            else
            {
                throw new ArgumentException("No vehicle with this license number was found");
            }
        }

        public void RechargeVehicle(string  i_LicenseNumber, float i_minutesToCharge)
        {
            if (r_Vehicles.TryGetValue(i_LicenseNumber, out OwnerInfo ownerInfo))
            {
                if()
            }
            else
            {
                throw new ArgumentException("No vehicle with this license number was found");
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
                throw new ArgumentException("No vehicle with this license number was found");
            }
        }
    }
}
