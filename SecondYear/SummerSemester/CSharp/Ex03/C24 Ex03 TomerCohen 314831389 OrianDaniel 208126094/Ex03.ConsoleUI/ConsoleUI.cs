using Ex03.GarageLogic;
using System;
using System.Collections.Generic;

namespace Ex03.ConsoleUI
{
    public class ConsoleUI
    {
        private readonly Garage r_Garage;
        private readonly VehicleCreator r_VehicleCreator;
        public ConsoleUI()
        {
            r_Garage = new Garage();
            r_VehicleCreator = new VehicleCreator();
        }
        private int displayMenuAndGetChoice()
        {
            const int minOption = 1;
            const int maxOption = 8;
            int userChoice = -1;
            bool isValidChoice;
            string menu = $@"
Welcome to the best garage!

Please choose an option:
1. Insert a new vehicle into the garage.
2. Show license numbers of vehicles by filter.
3. Change the status of a vehicle.
4. Inflate air in wheels.
5. Fill the gas tank.
6. Charge the battery.
7. Show vehicle details.
8. Exit garage
";
            Console.WriteLine(menu);

            do
            {
                Console.Write("Your choice: ");
                string input = Console.ReadLine();

                try
                {
                    if (!int.TryParse(input, out userChoice))
                    {
                        throw new FormatException("Input must be an integer.");
                    }

                    if (userChoice < minOption || userChoice > maxOption)
                    {
                        throw new ArgumentOutOfRangeException($"Choice must be between {minOption} and {maxOption}.");
                    }

                    isValidChoice = true;
                }
                catch (FormatException ex)
                {
                    Console.WriteLine($"Invalid input: {ex.Message}");
                    isValidChoice = false;
                }
                catch (ArgumentException ex)
                {
                    Console.WriteLine($"Invalid input: {ex.Message}");
                    isValidChoice = false;
                }
                catch (Exception ex)
                {
                    Console.WriteLine($"An unexpected error occurred: {ex.Message}");
                    isValidChoice = false;
                }

            } while (!isValidChoice);

            return userChoice;
        }

        public void GarageManager()
        {
            bool quit = false;
            int userChoice;

            while (!quit)
            {
                try
                {
                    userChoice = displayMenuAndGetChoice();

                    switch (userChoice)
                    {
                        case 1:
                            AddVehicle();
                            break;
                        case 2:
                            displayLicensesByFilter();
                            break;
                        case 3:
                            changeVehicleState();
                            break;
                        case 4:
                            inflateWheels();
                            break;
                        case 5:
                            fillGasTank();
                            break;
                        case 6:
                            chargeBattery();
                            break;
                        case 7:
                            showVehicleDetails();
                            break;
                        case 8:
                            quit = true;
                            break;
                        default:
                            throw new ArgumentException("Choice is not valid");
                    }
                }
                catch (Exception ex)
                {
                    if (ex is ValueOutOfRangeException)
                    {
                        Console.WriteLine((ex as ValueOutOfRangeException).InnerException.Message);
                    }

                    Console.WriteLine(ex.Message);
                }
            }
        }

        private string getLicenseNumber()
        {
            Console.WriteLine("Enter vehicle license number:");
            return Console.ReadLine();
        }
        private void showVehicleDetails()
        {
            string licenseNumber = getLicenseNumber();
            r_Garage.DisplayFullVehicleInfo(licenseNumber);
        }

        private void chargeBattery()
        {
            string licenseNumber = getLicenseNumber();

            Console.WriteLine("Enter number of minutes to charge:");
            if (!float.TryParse(Console.ReadLine(), out float hoursToCharge))
            {
                throw new FormatException("invalid hours input");
            }

            r_Garage.RechargeVehicle(licenseNumber, hoursToCharge);
            Console.WriteLine("The battery has been charged.");
        }

        private void fillGasTank()
        {
            string licenseNumber = getLicenseNumber();

            int maxChoice = Enum.GetValues(typeof(eFuelType)).Length;
            Console.WriteLine("Choose a fuel type for the vehicle:");
            DisplayEnumValues<eFuelType>();

            int fuelType;
            while (!int.TryParse(Console.ReadLine(), out fuelType) || fuelType < 1 || fuelType > maxChoice)
            {
                Console.WriteLine($"Please enter a valid choice between 1 and {maxChoice}.");
                Console.WriteLine("Choose a fuel type for the vehicle:");
                DisplayEnumValues<eFuelType>();
            }

            eFuelType fuelTypeEnum = (eFuelType)(fuelType - 1);

            Console.WriteLine("Enter amount of fuel for the refuel:");
            if (!float.TryParse(Console.ReadLine(), out float fuelAmount))
            {
                throw new FormatException("invalid fuel input");
            }

            r_Garage.RefuelVehicle(licenseNumber, fuelTypeEnum, fuelAmount);
            Console.WriteLine("The fuel tank has been filled.");
        }
        private void inflateWheels()
        {
            string licenseNumber = getLicenseNumber();

            r_Garage.InflateToMax(licenseNumber);
            Console.WriteLine("All the wheels are now full of air.");
        }
        private void changeVehicleState()
        {
            string licenseNumber = getLicenseNumber();

            int maxChoice = Enum.GetValues(typeof(eVehicleState)).Length;
            Console.WriteLine("Choose a status for the vehicle:");
            DisplayEnumValues<eVehicleState>();

            int vehicleState;
            while (!int.TryParse(Console.ReadLine(), out vehicleState) || vehicleState < 1 || vehicleState > maxChoice)
            {
                Console.WriteLine($"Please enter a valid choice between 1 and {maxChoice}.");
                Console.WriteLine("Choose one of our vehicle states:");
                DisplayEnumValues<eVehicleState>();
            }

            eVehicleState vehicleStateEnum = (eVehicleState)(vehicleState - 1);
            r_Garage.ChangeVehicleState(licenseNumber, vehicleStateEnum);
            Console.WriteLine("State has been updated.");
        }
        private void displayLicensesByFilter()
        {
            bool inRepair = ConvertYesNoToBool("Do you want to see vehicles in repair? (Y/N): ");
            bool repaired = ConvertYesNoToBool("Do you want to see vehicles that are repaired? (Y/N): ");
            bool paid = ConvertYesNoToBool("Do you want to see vehicles that are paid? (Y/N): ");

            Console.WriteLine(r_Garage.DisplayListOfLicenseNumbers(inRepair, repaired, paid));
        }

        private bool ConvertYesNoToBool(string i_UserInput)
        {
            string choice;

            do
            {
                Console.Write(i_UserInput);
                choice = Console.ReadLine();
            } while (choice != "Y" && choice != "N");

            return choice == "Y";
        }
        private void AddVehicle()
        {
            OwnerInfo ownerInfo = getOwnerInfo();

            if (r_Garage.AddVehicle(ownerInfo))
            {
                Console.WriteLine(ownerInfo.ToString());
                Console.WriteLine("This vehicle was added successfuly to the garage.");
            }
            else
            {
                Console.WriteLine("This vehicle is already in the garage.");
            }
        }
        private OwnerInfo getOwnerInfo()
        {
            Console.WriteLine("Please enter customer's name:");
            string name = Console.ReadLine();

            Console.WriteLine("Please enter customer's phone number:");
            string phoneNumber = Console.ReadLine();

            Vehicle vehicle = getVehicleInfo();

            return new OwnerInfo(name, phoneNumber, eVehicleState.InRepair, vehicle);
        }
        private Vehicle getVehicleInfo()
        {
            string licenseNumber = getLicenseNumber();

            Console.WriteLine("Please enter model name:");
            string modelName = Console.ReadLine();

            int maxChoice = Enum.GetValues(typeof(eVehiclesTypes)).Length;
            Console.WriteLine("Choose one of our vehicles:");
            DisplayEnumValues<eVehiclesTypes>();

            int vehicleType;
            while (!int.TryParse(Console.ReadLine(), out vehicleType) || vehicleType < 1 || vehicleType > maxChoice)
            {
                Console.WriteLine($"Please enter a valid choice between 1 and {maxChoice}.");
                Console.WriteLine("Choose one of our vehicles:");
                DisplayEnumValues<eVehiclesTypes>();
            }
            
            eVehiclesTypes vehicleTypeEnum = (eVehiclesTypes)(vehicleType - 1);
            Dictionary<eOptions, object> options = r_VehicleCreator.GetOptions(vehicleTypeEnum);
            getInfoForOptions(options);

            return r_VehicleCreator.CreateVehicle(vehicleTypeEnum, licenseNumber, options, modelName);
        }
        private void DisplayEnumValues<T>() where T : Enum
        {
            int index = 1;
            foreach (T value in Enum.GetValues(typeof(T)))
            {
                Console.WriteLine($"{index++}. {value}");
            }
        }
        private void getInfoForOptions(Dictionary<eOptions, object> i_Options)
        {
            if (i_Options.ContainsKey(eOptions.LicenseType))
            {
                Console.WriteLine("Choose License Type of your Motorcycle:");
                DisplayEnumValues<eLicenseType>();
                i_Options[eOptions.LicenseType] = getEnumValueFromUser<eLicenseType>();
            }

            if (i_Options.ContainsKey(eOptions.EngineVolume))
            {
                Console.WriteLine("Please enter Engine Volume:");
                if (int.TryParse(Console.ReadLine(), out int engineVolume))
                {
                    i_Options[eOptions.EngineVolume] = engineVolume;
                }
                else
                {
                    Console.WriteLine("Invalid input for Engine Volume. It must be an integer.");
                }
            }

            if (i_Options.ContainsKey(eOptions.Color))
            {
                Console.WriteLine("Choose car's Color:");
                DisplayEnumValues<eColors>();
                i_Options[eOptions.Color] = getEnumValueFromUser<eColors>();
            }

            if (i_Options.ContainsKey(eOptions.Doors))
            {
                Console.WriteLine("Choose Number of Doors:");
                DisplayEnumValues<eDoors>();
                i_Options[eOptions.Doors] = getEnumValueFromUser<eDoors>();
            }

            if (i_Options.ContainsKey(eOptions.CurrentFuel))
            {
                Console.WriteLine("Please enter Current Fuel Amount:");
                if (float.TryParse(Console.ReadLine(), out float currentFuel))
                {
                    i_Options[eOptions.CurrentFuel] = currentFuel;
                }
                else
                {
                    Console.WriteLine("Invalid input for Current Fuel. It must be a float.");
                }
            }

            if (i_Options.ContainsKey(eOptions.BatteryTimeRemaining))
            {
                Console.WriteLine("Please enter current hours remaining in battery:");
                if (float.TryParse(Console.ReadLine(), out float batteryTimeRemaining))
                {
                    i_Options[eOptions.BatteryTimeRemaining] = batteryTimeRemaining;
                }
                else
                {
                    Console.WriteLine("Invalid input for Battery Time Remaining. It must be a float.");
                }
            }

            if (i_Options.ContainsKey(eOptions.CurrentWheelPressure))
            {
                Console.WriteLine("Please enter Current Wheels Air Pressure:");
                if (float.TryParse(Console.ReadLine(), out float wheelPressure))
                {
                    i_Options[eOptions.CurrentWheelPressure] = wheelPressure;
                }
                else
                {
                    Console.WriteLine("Invalid input for Current Wheel Pressure. It must be a float.");
                }
            }

            if (i_Options.ContainsKey(eOptions.ManufacturerWheelName))
            {
                Console.WriteLine("Please enter Manufacturer wheel name:");
                i_Options[eOptions.ManufacturerWheelName] = Console.ReadLine();
            }

            if (i_Options.ContainsKey(eOptions.CarryDangerousMaterialsDang))
            {
                i_Options[eOptions.CarryDangerousMaterialsDang] = ConvertYesNoToBool("Does the Truck Contain Hazardous Materials (Y/N):");
            }

            if (i_Options.ContainsKey(eOptions.CargoVolume))
            {
                Console.WriteLine("What is the Truck Cargo Capacity?");
                if (float.TryParse(Console.ReadLine(), out float cargoVolume))
                {
                    i_Options[eOptions.CargoVolume] = cargoVolume;
                }
                else
                {
                    Console.WriteLine("Invalid input for Cargo Volume. It must be a float.");
                }
            }
        }

        private TEnum getEnumValueFromUser<TEnum>() where TEnum : Enum
        {
            int choice;
            int maxChoice = Enum.GetValues(typeof(TEnum)).Length;

            Console.WriteLine($"Please enter a choice between 1 and {maxChoice}:");

            while (true)
            {
                if (int.TryParse(Console.ReadLine(), out choice) && choice >= 1 && choice <= maxChoice)
                {
                    return (TEnum)(object)(choice - 1);
                }

                Console.WriteLine($"Invalid choice. Please enter a valid choice between 1 and {maxChoice}:");
            }
        }
    }
}
