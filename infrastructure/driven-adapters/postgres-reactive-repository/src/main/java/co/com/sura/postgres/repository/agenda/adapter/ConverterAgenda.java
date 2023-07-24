package co.com.sura.postgres.repository.agenda.adapter;


import co.com.sura.autoagendador.CitaGenetic;
import co.com.sura.entity.agenda.Actividad;
import co.com.sura.entity.agenda.Desplazamiento;
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
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.DesplazamientoData;
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
public class ConverterAgenda {


    public static Profesional convertToProfesional(ProfesionalData profesionalData){
        return new Profesional()
                .toBuilder()
                .numeroIdentificacion(profesionalData.getNumeroIdentificacion())
                .idTipoIdentificacion(profesionalData.getIdTipoIdentificacion())
                .nombres(profesionalData.getNombre())
                .apellidos(profesionalData.getApellido())
                .fechaNacimiento(profesionalData.getFechaNacimiento())
                .idCiudad(profesionalData.getIdCiudad())
                .activo(profesionalData.isActivo())
                .build();
    }
    public static Actividad convertToActividad(ProfesionalData profesionalData){
        return new Actividad()
                .toBuilder()
                .responsable(profesionalData.getNombre()+" "+profesionalData.getApellido())
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
                .id(String.valueOf(desplazamientoData.getIdDesplazamiento()))
                .fechaInicio(desplazamientoData.getFechaProgramada())
                .fechaProgramada(desplazamientoData.getFechaProgramada())
                .tipo("dvisita")
                .duracion(desplazamientoData.getDuracion())
                .holgura(desplazamientoData.getHolgura())
                .build();
    }

    public static ProfesionalData convertToProfesionalData(Profesional profesional){
        return new ProfesionalData()
                .toBuilder()
                .numeroIdentificacion(profesional.getNumeroIdentificacion())
                .idTipoIdentificacion(profesional.getIdTipoIdentificacion())
                .nombre(profesional.getNombres())
                .apellido(profesional.getApellidos())
                .fechaNacimiento(profesional.getFechaNacimiento())
                .idCiudad(profesional.getIdCiudad())
                .activo(profesional.isActivo())
                .build();
    }

    public static Cita convertToCita(CitaData citaData){
        return new Cita()
                .toBuilder()
                .idCita(citaData.getIdCita())
                .idRemision(citaData.getIdRemision())
                .duracion(citaData.getDuracion())
                .holgura(citaData.getHolgura())
                .fechaInicio(citaData.getFechaInicio())
                .fechaProgramada(citaData.getFechaProgramada())
                .especialidad(citaData.getEspecialidad())
                .idEstado(citaData.getIdEstado())
                .idCiudad(citaData.getIdCiudad())
                .idProfesional(citaData.getIdProfesional())
                .idConductor(citaData.getIdConductor())
                .latitud(citaData.getLatitud())
                .longitud(citaData.getLongitud())
                .build();
    }
    public static  List<CitaGenetic> convertToListCitaGenetic(List<CitaData> citas){
        return citas
                .stream()
                .map(ConverterAgenda::convertToCitaGenetic)
                .collect(Collectors.toList());
    }
    public static CitaGenetic convertToCitaGenetic(CitaData citaData){
        return new CitaGenetic()
                .toBuilder()
                .idCita(citaData.getIdCita())
                .duracion(citaData.getDuracion())
                .holgura(citaData.getHolgura())
                .fechaInicioIso(citaData.getFechaProgramada().toEpochSecond(ZoneOffset.UTC))
                .latitud(citaData.getLatitud())
                .longitud(citaData.getLongitud())
                .build();
    }
    public static Desplazamiento converToDesplazamiento(DesplazamientoData desplazamientoData){
            return new Desplazamiento()
                    .toBuilder()
                    .idCitaPartida(desplazamientoData.getIdCitaPartida())
                    .idCitaDestino(desplazamientoData.getIdCitaDestino())
                    .tipo(desplazamientoData.getTipo())
                    .duracion(desplazamientoData.getDuracion())
                    .holgura(desplazamientoData.getHolgura())
                    .fechaProgramada(desplazamientoData.getFechaProgramada())
                    .idHorarioTurno(desplazamientoData.getIdHorarioTurno())
                    .build();
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
        return new Curacion()
                .toBuilder()
                .tipoCuracion(curacionData.getTipoCuracion())
                .descripcion(curacionData.getDescripcion())
                .sesiones(curacionData.getSesiones())
                .build();
    }

    public static Canalizacion convertToCanalizacion(CanalizacionData canalizacionData){
        return new Canalizacion()
                .toBuilder()
                .tipoCanalizacion(canalizacionData.getTipoCanalizacion())
                .tipoPrestacion(canalizacionData.getTipoPrestacion())
                .build();
    }

    public static Fototerapia convertToFototerapia(FototerapiaData fototerapiaData){
        return new Fototerapia()
                .toBuilder()
                .diasTratamiento(fototerapiaData.getDiasTratamiento())
                .tipoFrecuencia(fototerapiaData.getTipofrecuencia())
                .cantidadDosis(fototerapiaData.getCantidadDosis())
                .tipoPrestacion(fototerapiaData.getTipoPrestacion())
                .build();
    }

    public static Secrecion convertToSecrecionData(SecrecionData secrecionData){
        return new Secrecion()
                .toBuilder()
                .diasTratamiento(secrecionData.getDiasTratamiento())
                .envioAspirador(secrecionData.isEnvioAspirador())
                .visitaEnfermeria(secrecionData.isVisitaEnfermeria())
                .tipoSonda(secrecionData.getTipoSonda())
                .nasal(secrecionData.isNasal())
                .traqueostomia(secrecionData.isTraqueostomia())
                .tipoPrestacion(secrecionData.getTipoPrestacion())
                .build();
    }

    public static Sondaje convertToSondajeData(SondajeData sondajeData){
        return new Sondaje()
                .toBuilder()
                .tipoSondaje(sondajeData.getTipoSondaje())
                .tipoSonda(sondajeData.getSondaje())
                .totalSesiones(sondajeData.getTotalSesiones())
                .tipoPrestacion(sondajeData.getTipoPrestacion())
                .build();
    }
    public static TomaMuestra convertToTomaMuestraData(TomaMuestraData tomaMuestraData){
        return new TomaMuestra()
                .toBuilder()
                .tipoMuestra(tomaMuestraData.getTipoMuestra())
                .requiereAyuno(tomaMuestraData.getRequiereAyuno())
                .tipoPrestacion(tomaMuestraData.getTipoPrestacion())
                .build();
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
