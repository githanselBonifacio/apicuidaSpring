package co.com.sura.postgres.repository.agenda.adapter;


import co.com.sura.autoagendador.CitaGenetic;
import co.com.sura.entity.agenda.Actividad;
import co.com.sura.entity.moviles.Desplazamiento;
import co.com.sura.entity.agenda.Profesional;
import co.com.sura.entity.agenda.Tarea;
import co.com.sura.entity.remision.Tratamiento;
import co.com.sura.entity.remision.Fototerapia;
import co.com.sura.entity.remision.TomaMuestra;
import co.com.sura.entity.remision.Sondaje;
import co.com.sura.entity.remision.Canalizacion;
import co.com.sura.entity.remision.Cita;
import co.com.sura.entity.remision.Curacion;
import co.com.sura.entity.remision.SoporteNutricional;
import co.com.sura.entity.remision.Secrecion;
import co.com.sura.entity.remision.Medicamento;
import co.com.sura.postgres.Converter;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.moviles.data.DesplazamientoData;
import co.com.sura.postgres.repository.agenda.data.ProfesionalData;
import co.com.sura.postgres.repository.remision.data.TratamientoData;
import co.com.sura.postgres.repository.remision.data.CanalizacionData;
import co.com.sura.postgres.repository.remision.data.CuracionData;
import co.com.sura.postgres.repository.remision.data.TomaMuestraData;
import co.com.sura.postgres.repository.remision.data.SondajeData;
import co.com.sura.postgres.repository.remision.data.SecrecionData;
import co.com.sura.postgres.repository.remision.data.FototerapiaData;
import co.com.sura.postgres.repository.remision.data.SoporteNutricionalData;
import org.springframework.stereotype.Component;

import java.time.ZoneOffset;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class ConverterAgenda extends Converter {


    public static Profesional convertToProfesional(ProfesionalData profesionalData){
        return deserializarJson(
                convertirObjetoAJson(profesionalData), Profesional.class
        );
    }

    public static ProfesionalData convertToProfesionalData(Profesional profesional){
        return deserializarJson(
                convertirObjetoAJson(profesional), ProfesionalData.class
        );
    }

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
                .tipo("dvisita")
                .duracion(desplazamientoData.getDuracion())
                .holgura(desplazamientoData.getHolgura())
                .build();
    }

    public static Cita convertToCita(CitaData citaData){
        var cita =  deserializarJson(
                convertirObjetoAJson(citaData), Cita.class
        );
        //mientra se vuelven a crear las citas sin el _
        cita.setIdCita(citaData.getIdCita());
        return cita;
    }
    public static  List<CitaGenetic> convertToListCitaGenetic(List<Cita> citas){
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
    public static Desplazamiento converToDesplazamiento(DesplazamientoData desplazamientoData){
        return deserializarJson(
                convertirObjetoAJson(desplazamientoData), Desplazamiento.class
        );
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
        return deserializarJson(
                convertirObjetoAJson(curacionData), Curacion.class
        );

    }

    public static Canalizacion convertToCanalizacion(CanalizacionData canalizacionData){
        return deserializarJson(
                convertirObjetoAJson(canalizacionData), Canalizacion.class
        );
    }

    public static Fototerapia convertToFototerapia(FototerapiaData fototerapiaData){
        return deserializarJson(
                convertirObjetoAJson(fototerapiaData), Fototerapia.class
        );
    }

    public static Secrecion convertToSecrecionData(SecrecionData secrecionData){
        return deserializarJson(
                convertirObjetoAJson(secrecionData), Secrecion.class
        );
    }

    public static Sondaje convertToSondajeData(SondajeData sondajeData){
        return deserializarJson(
                convertirObjetoAJson(sondajeData), Sondaje.class
        );
    }
    public static TomaMuestra convertToTomaMuestraData(TomaMuestraData tomaMuestraData){
        return deserializarJson(
                convertirObjetoAJson(tomaMuestraData), TomaMuestra.class
        );
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
}
