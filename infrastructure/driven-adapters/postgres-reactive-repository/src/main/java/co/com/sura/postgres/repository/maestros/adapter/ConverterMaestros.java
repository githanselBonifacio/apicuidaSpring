package co.com.sura.postgres.repository.maestros.adapter;


import co.com.sura.entity.maestro.*;
import co.com.sura.postgres.repository.maestros.data.*;
import org.springframework.stereotype.Component;

@Component
public class ConverterMaestros  {

    public static Ciudad convertToCiudad(CiudadData ciudadData){
        return new Ciudad()
                .toBuilder()
                .id(ciudadData.getId())
                .nombre(ciudadData.getNombre())
                .latitud(ciudadData.getLatitud())
                .longitud(ciudadData.getLongitud())
                .direccion(ciudadData.getDireccion())
                .build();
    }

    public static HorarioTurno convertToHorarioTurno(HorarioTurnoData horarioTurnoData){
        return new HorarioTurno()
                .toBuilder()
                .id(horarioTurnoData.getId())
                .nombre(horarioTurnoData.getNombre())
                .horaInicio(horarioTurnoData.getHoraInicio())
                .horaFin(horarioTurnoData.getHoraFin())
                .build();
    }

    public static TipoIdentificacion convertToTipoIdentificacion(TipoIdentificacionData tipoIdentificacionData){
        return new TipoIdentificacion()
                .toBuilder()
                .id(tipoIdentificacionData.getId())
                .idTipo(tipoIdentificacionData.getIdTipo())
                .nombre(tipoIdentificacionData.getNombre())
                .build();
    }

    public static PlanSalud convertToTipoIdentificacion(PlanSaludData planSaludData){
        return new PlanSalud()
                .toBuilder()
                .id(planSaludData.getId())
                .idPlan(planSaludData.getIdPlan())
                .nombre(planSaludData.getNombre())
                .nombrePrestador(planSaludData.getNombrePrestador())
                .build();
    }
    public static EstadoCita convertToEstadoCita(EstadoCitaData estadoCitaData){
        return new EstadoCita()
                .toBuilder()
                .id(estadoCitaData.getId())
                .nombre(estadoCitaData.getNombre())
                .build();
    }
}
