package co.com.sura.postgres.repository.remision.adapter;


import co.com.sura.dto.remision.*;
import co.com.sura.entity.remision.*;
import co.com.sura.postgres.repository.agenda.data.CitaData;
import co.com.sura.postgres.repository.remision.data.*;
import org.springframework.stereotype.Component;
import reactor.core.publisher.Flux;

import java.util.Collection;
import java.util.List;
import java.util.stream.Collectors;

@Component
public class ConverterRemision {

    public static Remision convertToRemision(RemisionData remisionData){
        return new Remision();
    }
    public static RemisionData convertToRemisionData(Remision remision){
        return new RemisionData();
    }
    public static RemisionData convertToRemisionRequest(RemisionRequest remisionRequest){

        return new RemisionData()
                .toBuilder()
                .idRemision(remisionRequest.getIdRemision())
                .institucionRemite(remisionRequest.getInstitucionRemite())
                .programa(remisionRequest.getPrograma().getNombre())
                .fechaAdmision(remisionRequest.getFechaAdmision())
                .tipoAdmision(remisionRequest.getTipoAdmision())
                .numeroIdentificacionPaciente(remisionRequest.getNumeroIdentificacion())
                .estado(remisionRequest.getEstado())
                .build();
    }
    public static UbicacionData extraerUbicacionData(RemisionRequest remisionRequest){
        UbicacionRequest ubicacionRequest = remisionRequest.getDatosAtencionPaciente().getUbicacion();
        return new UbicacionData()
                .toBuilder()
                .idUbicacion(remisionRequest.getIdRemision()+"_ubicacion")
                .latitud(ubicacionRequest.getLatitud())
                .longitud(ubicacionRequest.getLongitud())
                .direccion(ubicacionRequest.getDireccion())
                .tipoVia(ubicacionRequest.getTipoVia())
                .numero1(ubicacionRequest.getNumero1())
                .numeroInterseccion(ubicacionRequest.getNroInterseccion())
                .numero2(ubicacionRequest.getNumero2())
                .barrio(ubicacionRequest.getBarrio())
                .sinNomenclatura(ubicacionRequest.isSinNomenclatura())
                .municipio(ubicacionRequest.getMunicipio().getNombre())
                .idCiudad(ubicacionRequest.getMunicipio().getIdCiudad())
                .build();
    }

    public static PacienteData extraerPacienteData(RemisionRequest remisionRequest){
        return new PacienteData()
                .toBuilder()
                .tipoIdentificacion(remisionRequest.getTipoIdentificacion().getNombre())
                .numeroIdentificacion(remisionRequest.getNumeroIdentificacion())
                .nombre(remisionRequest.getNombre())
                .apellido(remisionRequest.getApellido())
                .edad(remisionRequest.getEdad())
                .sexo(remisionRequest.getSexo())
                .peso(remisionRequest.getPeso())
                .tipoAfiliacion(remisionRequest.getTipoIdentificacion().getNombre())
                .nombreAseguradora(remisionRequest.getTipoAfiliacion().getNombreAseguradora())
                .fechaNacimiento(remisionRequest.getFechaNacimiento())
                .idUbicacion(remisionRequest.getIdRemision()+"_ubicacion")
                .build();
    }
    public static List<RemisionDiagnosticoData> extraerRemisionDiagnosticoData(
            List<Diagnostico> diagnosticos, String idRemision){

        return diagnosticos
                .stream()
                .map(diagnostico -> {
                    return new RemisionDiagnosticoData()
                            .toBuilder()
                            .idRemision(idRemision)
                            .nombreDiagnostico(diagnostico.getNombre())
                            .build();
                })
                .collect(Collectors.toList());
    }

    public static CitaData convertirCitaData(CitaRequest citaRequest){
        return new CitaData()
                .toBuilder()
                .idCita(citaRequest.getIdCita())
                .duracion(citaRequest.getDuracion())
                .holgura(citaRequest.getHolgura())
                .fechaInicio(citaRequest.getFechaInicio())
                .especialidad(citaRequest.getEspecialidad())
                .build();
    }

    public static List<CitaData> convertirCitasDataList(
            List<CitaRequest> citasRequest, String idRemision, String idCiudad){

        return citasRequest
                .stream()
                .map(ConverterRemision :: convertirCitaData)
                .peek(citaData -> citaData.setIdRemision(idRemision))
                .peek(citaData -> citaData.setIdCiudad(idCiudad))
                .collect(Collectors.toList());
    }
    public static DatosAtencionPacienteData convertirDatosAtencionPacienteData(
            DatosAtencionPaciente datosAtencionPaciente, String idRemision){
        return new DatosAtencionPacienteData()
                .toBuilder()
                .nombreCuidador(datosAtencionPaciente.getNombreCuidador())
                .nombreResponsable(datosAtencionPaciente.getNombreResponsable())
                .telefonoPaciente(datosAtencionPaciente.getTelefonoPaciente())
                .celularPaciente(datosAtencionPaciente.getCelularPaciente())
                .celularPaciente2(datosAtencionPaciente.getCelularPaciente2())
                .idRemision(idRemision)
                .build();
    }
    protected static  TratamientoData convetirTratamientoData (TratamientoRequest tratamientoRequest){
        return new TratamientoData()
                .toBuilder()
                .tipoTratamiento(tratamientoRequest.getTipoTratamiento().getNombre())
                .idMedicamento(tratamientoRequest.getMedicamento().getIdMedicamento())
                .cantidadDosis((int) tratamientoRequest.getCantidadDosis())
                .unidadDosis(tratamientoRequest.getUnidadDosis().getDescripcion())
                .viaAdministracion(tratamientoRequest.getViaAdministracion().getDescripcion())
                .frecuencia(tratamientoRequest.getFrecuencia().getDescripcion())
                .duracion((int) tratamientoRequest.getDuracion())
                .noPBS(tratamientoRequest.getNoPBS())
                .tipoPrestacion(tratamientoRequest.getTipoPrestacion().getTipoPrestacion())
                .build();
    }
    public  static  List<TratamientoData> extraerTratamientoData(List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getTratamientos()
                        .stream()
                        .map(ConverterRemision ::convetirTratamientoData)
                        .peek(tratamientoData -> tratamientoData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static CanalizacionData convertirCanalizacionData(Canalizacion canalizacion ){
        return new CanalizacionData()
                .toBuilder()
                .tipoCanalizacion(canalizacion.getTipoCanalizacion())
                .tipoPrestacion(canalizacion.getTipoPrestacion())
                .build();
    }

    public  static  List<CanalizacionData> extraerCanalizacionData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getCanalizaciones()
                        .stream()
                        .map(ConverterRemision :: convertirCanalizacionData)
                        .peek(canalizacionData -> canalizacionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static FototerapiaData convertirFototerapiaData(Fototerapia fototerapia ){
        return new FototerapiaData()
                .toBuilder()
                .diasTratamiento(fototerapia.getDiasTratamiento())
                .cantidadDosis(fototerapia.getCantidadDosis())
                .tipofrecuencia(fototerapia.getTipoFrecuencia())
                .tipoPrestacion(fototerapia.getTipoPrestacion())
                .build();
    }

    public  static  List<FototerapiaData> extraerFototerapiaData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getFototerapias()
                        .stream()
                        .map(ConverterRemision :: convertirFototerapiaData)
                        .peek(fototerapiaData -> fototerapiaData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static SecrecionData convertirSecrecionData(SecrecionRequest secrecionRequest ){
        return new SecrecionData()
                .toBuilder()
                .diasTratamiento(Integer.parseInt(secrecionRequest.getDiasTratamiento()))
                .envioAspirador(secrecionRequest.isEnvioAspirador())
                .isNasal(secrecionRequest.isNasal())
                .isTraqueostomia(secrecionRequest.isTraqueostomia())
                .visitaEnfermeria(secrecionRequest.isVisitaEnfermeria())
                .tipoSonda(secrecionRequest.getTipoSonda())
                .tipoPrestacion(secrecionRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<SecrecionData> extraerSecrecionData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSecreciones()
                        .stream()
                        .map(ConverterRemision :: convertirSecrecionData)
                        .peek(secrecionData -> secrecionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static SondajeData convertirSondajeData(SondajeRequest sondajeRequest ){
        return new SondajeData()
                .toBuilder()
                .sondaje(sondajeRequest.getSondaje())
                .tipoSondaje(sondajeRequest.getTipoSondaje())
                .totalSesiones(sondajeRequest.getTotalSesiones())
                .tipoPrestacion(sondajeRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<SondajeData> extraerSondajeData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSondajes()
                        .stream()
                        .map(ConverterRemision :: convertirSondajeData)
                        .peek(sondajeData -> sondajeData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }
    protected static SoporteNutricionalData convertirSoporteNutricional(
            SoporteNutricionalRequest soporteNutricionalRequest ){
        return new SoporteNutricionalData()
                .toBuilder()
                .descripcion(soporteNutricionalRequest.getDescripcion())
                .tipo(soporteNutricionalRequest.getTipo())
                .unidadDosis(soporteNutricionalRequest.getUnidadDosis().getDescripcion())
                .duracion(soporteNutricionalRequest.getDuracion())
                .volumen(soporteNutricionalRequest.getVolumen())
                .idMedicamento(soporteNutricionalRequest.getMedicamento().getIdMedicamento())
                .cantidadDosis(soporteNutricionalRequest.getCantidadDosis())
                .noPBS(soporteNutricionalRequest.isNoPBS())
                .tipoPrestacion(soporteNutricionalRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<SoporteNutricionalData> extraerSoporteNutricionalData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getSoporteNutricionales()
                        .stream()
                        .map(ConverterRemision :: convertirSoporteNutricional)
                        .peek(soporteNutricionalData -> soporteNutricionalData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }
    protected static TomaMuestraData convertirSoporteNutricional(TomaMuestraRequest tomaMuestraRequest ){
        return new TomaMuestraData()
                .toBuilder()
                .tipoMuestra(tomaMuestraRequest.getTipoMuestra().getDescripcion())
                .requiereAyuno(tomaMuestraRequest.isRequiereAyuno())
                .tipoPrestacion(tomaMuestraRequest.getTipoPrestacion())
                .build();
    }

    public  static  List<TomaMuestraData> extraerSoporteTomaMuestraData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getTomaMuestras()
                        .stream()
                        .map(ConverterRemision :: convertirSoporteNutricional)
                        .peek(tomaMuestraData ->  tomaMuestraData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }

    protected static CuracionData convertirCuracionData(CuracionRequest curacionRequest ){
        return new CuracionData()
                .toBuilder()
                .tipoCuracion(curacionRequest.getTipoCuracion().getDescripcion())
                .descripcion(curacionRequest.getDescripcion())
                .sesiones(curacionRequest.getSesiones())
                .build();
    }

    public  static  List<CuracionData> extraerCuracionData (List<CitaRequest> listacitasRequest) {
        return listacitasRequest
                .stream()
                .map(citaRequest -> citaRequest.getProcedimientos().getCuraciones()
                        .stream()
                        .map(ConverterRemision :: convertirCuracionData)
                        .peek(curacionData ->  curacionData.setIdCita(citaRequest.getIdCita()))
                        .collect(Collectors.toList()))
                .flatMap(Collection::parallelStream)
                .collect(Collectors.toList());
    }
}
