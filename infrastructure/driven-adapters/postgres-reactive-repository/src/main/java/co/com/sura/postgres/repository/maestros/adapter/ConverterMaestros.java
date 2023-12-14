package co.com.sura.postgres.repository.maestros.adapter;


import co.com.sura.maestros.entity.*;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.repository.maestros.data.*;
import org.springframework.stereotype.Component;

@Component
public class ConverterMaestros  extends Converter {

    public static Regional convertToCiudad(RegionalesData ciudadData){
        return deserializarJson(
                convertirObjetoAJson(ciudadData), Regional.class
        );
    }

    public static HorarioTurno convertToHorarioTurno(HorarioTurnoData horarioTurnoData){
        return deserializarJson(
                convertirObjetoAJson(horarioTurnoData), HorarioTurno.class
        );
    }

    public static TipoIdentificacion convertToTipoIdentificacion(TipoIdentificacionData tipoIdentificacionData){
        return deserializarJson(
                convertirObjetoAJson(tipoIdentificacionData), TipoIdentificacion.class
        );
    }

    public static EstadoCita convertToEstadoCita(EstadoCitaData estadoCitaData){
        return deserializarJson(
                convertirObjetoAJson(estadoCitaData), EstadoCita.class
        );
    }
    public static Profesion converToProfesionData(ProfesionData profesionData){
        return deserializarJson(
                convertirObjetoAJson(profesionData),Profesion.class
        );
    }
}
