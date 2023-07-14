package co.com.sura.postgres.repository.agenda.adapter;

import co.com.sura.entity.agenda.Profesional;
import co.com.sura.entity.remision.*;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.agenda.data.ProfesionalData;
import co.com.sura.postgres.repository.remision.data.*;
import org.springframework.stereotype.Component;

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
                .duracion(citaData.getDuracion())
                .holgura(citaData.getHolgura())
                .fechaInicio(citaData.getFechaInicio())
                .especialidad(citaData.getEspecialidad())
                .idEstado(citaData.getIdEstado())
                .idCiudad(citaData.getIdCiudad())
                .idProfesional(citaData.getIdProfesional())
                .idConductor(citaData.getIdConductor())
                .build();
    }

    public static Tratamiento convertToTratamiento(TratamientoData tratamientoData){
        return new Tratamiento()
                .toBuilder()
                .tipoTratamiento(tratamientoData.getTipoTratamiento())
                .medicamento(Medicamento.builder().idMedicamento(tratamientoData.getIdMedicamento()).build())
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
                        .idMedicamento(soporteNutricionalData.getIdMedicamento()).build())
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
