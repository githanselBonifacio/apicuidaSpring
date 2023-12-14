package co.com.sura.postgres.repository.agenda.adapter;


import co.com.sura.autoagendador.models.CitaGenetic;
import co.com.sura.agenda.entity.Actividad;
import co.com.sura.agenda.entity.Cita;
import co.com.sura.agenda.entity.Tarea;
import co.com.sura.moviles.entity.Desplazamiento;
import co.com.sura.personal.entity.TurnoProfesional;
import co.com.sura.remision.entity.datosremision.Tratamiento;
import co.com.sura.remision.entity.historial.CitaHistorial;
import co.com.sura.remision.entity.procedimientos.Fototerapia;
import co.com.sura.remision.entity.procedimientos.TomaMuestra;
import co.com.sura.remision.entity.procedimientos.Sondaje;
import co.com.sura.remision.entity.procedimientos.Canalizacion;
import co.com.sura.remision.entity.procedimientos.Curacion;
import co.com.sura.remision.entity.procedimientos.SoporteNutricional;
import co.com.sura.remision.entity.procedimientos.Secrecion;
import co.com.sura.remision.entity.datosremision.Medicamento;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoData;
import co.com.sura.postgres.repository.personal.data.ProfesionalData;
import co.com.sura.postgres.repository.personal.data.TurnoProfesionalesData;
import co.com.sura.postgres.repository.remision.data.TratamientoData;
import co.com.sura.postgres.repository.remision.data.CanalizacionData;
import co.com.sura.postgres.repository.remision.data.CuracionData;
import co.com.sura.postgres.repository.remision.data.TomaMuestraData;
import co.com.sura.postgres.repository.remision.data.SondajeData;
import co.com.sura.postgres.repository.remision.data.SecrecionData;
import co.com.sura.postgres.repository.remision.data.FototerapiaData;
import co.com.sura.postgres.repository.remision.data.SoporteNutricionalData;
import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

public class ConverterAgenda extends Converter {

    public static Actividad convertToActividad(ProfesionalData profesionalData){
        return new Actividad()
                .toBuilder()
                .responsable(profesionalData.getNombres()+" "+profesionalData.getApellidos())
                .numeroIdentificacion(profesionalData.getNumeroIdentificacion())
                .tareas(new ArrayList<>())
                .build();
    }
    public static Tarea convertToTarea(CitaData citaData){
        return new Tarea()
                .toBuilder()
                .id(citaData.getIdCita())
                .fechaInicio(citaData.getFechaInicio())
                .fechaProgramada(citaData.getFechaProgramada())
                .duracion(citaData.getDuracion())
                .holgura(citaData.getHolgura())
                .latitud(citaData.getLatitud())
                .longitud(citaData.getLongitud())
                .idEstado(citaData.getIdEstado())
                .build();
    }
    public static Tarea convertToTarea(DesplazamientoData desplazamientoData){
        return new Tarea()
                .toBuilder()
                .id(desplazamientoData.getIdCitaPartida()+" -> "+desplazamientoData.getIdCitaDestino())
                .fechaInicio(desplazamientoData.getFechaProgramada())
                .fechaProgramada(desplazamientoData.getFechaProgramada())
                .tipo(desplazamientoData.getTipo())
                .duracion(desplazamientoData.getDuracion())
                .holgura(desplazamientoData.getHolgura())
                .build();
    }
    public static  List<CitaGenetic> convertToListCitaGenetic(Collection<Cita> citas){
        return citas
                .stream()
                .map(ConverterAgenda::convertToCitaGenetic)
                .collect(Collectors.toList());
    }
    public static CitaGenetic convertToCitaGenetic(Cita cita){
        return new CitaGenetic()
                .toBuilder()
                .idCita(cita.getIdCita())
                .duracion(cita.getDuracion())
                .holgura(cita.getHolgura())
                .fechaInicioIso(cita.getFechaProgramada().toEpochSecond(ZoneOffset.UTC))
                .latitud(cita.getLatitud())
                .longitud(cita.getLongitud())
                .build();
    }

    public static CitaHistorial convertToCitaHistorial(CitaData citaData){
        return CitaHistorial.builder()
                .idCita(citaData.getIdCita())
                .idRemision(citaData.getIdRemision())
                .duracion(citaData.getDuracion())
                .idEstado(citaData.getIdEstado())
                .holgura(citaData.getHolgura())
                .fechaInicio(citaData.getFechaInicio())
                .fechaProgramada(citaData.getFechaProgramada())
                .especialidad(citaData.getEspecialidad())
                .idRegional(citaData.getIdRegional())
                .idProfesional(citaData.getIdProfesional())
                .idConductor(citaData.getIdConductor())
                .latitud(citaData.getLatitud())
                .longitud(citaData.getLongitud())
                .build();
    }

    public static Desplazamiento converToDesplazamiento(DesplazamientoData desplazamientoData){
        return converToEntity(desplazamientoData, Desplazamiento.class);
    }
    public static Tratamiento convertToTratamiento(TratamientoData tratamientoData){
        return new Tratamiento()
                .toBuilder()
                .tipoTratamiento(tratamientoData.getTipoTratamiento())
                .medicamento(
                        new Medicamento()
                                .toBuilder()
                                .idMedicamento(tratamientoData.getIdMedicamento())
                                .nombre(tratamientoData.getNombreMedicamento())
                                .presentacion(tratamientoData.getPresentacionMedicamento())
                                .codigoMedicamento(tratamientoData.getCodigoMedicamento())
                                .build()
                )
                .cantidadDosis(tratamientoData.getCantidadDosis())
                .unidadDosis(tratamientoData.getUnidadDosis())
                .viaAdministracion(tratamientoData.getViaAdministracion())
                .frecuencia(tratamientoData.getFrecuencia())
                .duracion(tratamientoData.getDuracion())
                .noPBS(tratamientoData.getNoPBS())
                .tipoPrestacion(tratamientoData.getTipoPrestacion())
                .build();
    }

    public static Curacion convertToCuracion(CuracionData curacionData){
        return converToEntity(curacionData, Curacion.class);

    }

    public static Canalizacion convertToCanalizacion(CanalizacionData canalizacionData){
        return converToEntity(canalizacionData, Canalizacion.class);
    }

    public static Fototerapia convertToFototerapia(FototerapiaData fototerapiaData){
        return converToEntity(fototerapiaData, Fototerapia.class);
    }

    public static Secrecion convertToSecrecionData(SecrecionData secrecionData){
        return converToEntity(secrecionData, Secrecion.class);
    }

    public static Sondaje convertToSondajeData(SondajeData sondajeData){
        return converToEntity(sondajeData, Sondaje.class);
    }
    public static TomaMuestra convertToTomaMuestraData(TomaMuestraData tomaMuestraData){
        return converToEntity(tomaMuestraData, TomaMuestra.class);
    }
    public static SoporteNutricional convertToSoporteNutricionalData(SoporteNutricionalData soporteNutricionalData){
        return new SoporteNutricional()
                .toBuilder()
                .medicamento(
                        new Medicamento()
                        .toBuilder()
                                .idMedicamento(soporteNutricionalData.getIdMedicamento())
                                .nombre(soporteNutricionalData.getNombreMedicamento())
                                .presentacion(soporteNutricionalData.getPresentacionMedicamento())
                                .codigoMedicamento(soporteNutricionalData.getCodigoMedicamento())
                                .build()
                )
                .cantidadDosis(soporteNutricionalData.getCantidadDosis())
                .unidadDosis(soporteNutricionalData.getUnidadDosis())
                .tipo(soporteNutricionalData.getTipo())
                .descripcion(soporteNutricionalData.getDescripcion())
                .duracion(soporteNutricionalData.getDuracion())
                .volumen(soporteNutricionalData.getVolumen())
                .noPBS(soporteNutricionalData.getNoPBS())
                .tipoPrestacion(soporteNutricionalData.getTipoPrestacion())
                .build();
    }

    public static TurnoProfesionalesData converToTurnoProfesionalData(TurnoProfesional turnoProfesional){
        return converToEntity(turnoProfesional, TurnoProfesionalesData.class);
    }
}
