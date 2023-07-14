package co.com.sura.postgres.repository.maestros.adapter;


import co.com.sura.entity.maestro.Ciudad;
import co.com.sura.entity.maestro.HorarioTurno;
import co.com.sura.entity.maestro.PlanSalud;
import co.com.sura.entity.maestro.TipoIdentificacion;
import co.com.sura.postgres.repository.maestros.data.CiudadData;
import co.com.sura.postgres.repository.maestros.data.HorarioTurnoData;
import co.com.sura.postgres.repository.maestros.data.PlanSaludData;
import co.com.sura.postgres.repository.maestros.data.TipoIdentificacionData;
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
}
