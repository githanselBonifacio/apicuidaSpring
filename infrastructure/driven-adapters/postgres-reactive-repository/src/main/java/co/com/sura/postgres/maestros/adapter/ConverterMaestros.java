package co.com.sura.postgres.maestros.adapter;


import co.com.sura.maestros.entity.*;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.maestros.data.*;
import org.springframework.stereotype.Component;

@Component
public class ConverterMaestros  extends Converter {

    public static Regional convertToRegional(RegionalData regionalData){
        return deserializarJson(
                convertirObjetoAJson(regionalData), Regional.class
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
    public static MotivoCancelacionCita converToMotivoCancelacionCita(MotivoCancelacionCitaData motivoCancelacionCitaData){
        return deserializarJson(
                convertirObjetoAJson(motivoCancelacionCitaData),MotivoCancelacionCita.class
        );
    }
}

