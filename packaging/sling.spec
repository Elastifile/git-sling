Name:           sling
Version:        PKG_VERSION
Release:        PKG_RELEASE%{?dist}
Summary:        A lightweight continuous integration tool for git

Group:          System Environment/Libraries
License:        GPLv2
URL:            https://github.com/Elastifile/git-sling
Source0:        %{name}-%{version}.tar.gz
BuildRoot:      %(mktemp -ud %{_tmppath}/%{name}-%{version}-%{release}-XXXXXX)

# NOTE: This spec does not adhere to GHC packaging under Fedora.
# Instead, it uses Stack from the internet.
BuildRequires:  zlib-devel
BuildRequires:  gmp-devel
BuildRequires:  python
BuildRequires:  chrpath
BuildRequires:  wget

%if 0%{?fedora} >= 24
# GHC builds need tinfo.so.5
BuildRequires:  ncurses-compat-libs
BuildRequires:  glibc-langpack-en
%endif

Requires:       gmp

%description
Sling is a lightweight continuous integration tool for git

%global debug_package %{nil}

%prep
%setup -q

mkdir -p ~/.local/{bin,stack}
export PATH=~/.local/bin:$PATH

if [[ ! -x ~/.local/bin/stack ]] ; then
    wget -O - https://github.com/commercialhaskell/stack/releases/download/v1.7.1/stack-1.7.1-linux-x86_64.tar.gz | \
	tar -zxf - -C ~/.local/stack
    ln -f -s ~/.local/stack/stack-1.7.1-linux-x86_64/stack ~/.local/bin/stack
fi

cd server
stack --no-terminal setup

%build

# It depends on git, we need to fix that. For now:
export SLING_BUILT_REVISION=%{version}-%{release}
export PATH=~/.local/bin:$PATH

cd server
stack --no-terminal build

%install

echo rpm build root: $RPM_BUILD_ROOT
echo prefix is: %{_prefix}

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/share/doc/sling-0.1.0.0
cp server/LICENSE $RPM_BUILD_ROOT/%{_prefix}/share/doc/sling-0.1.0.0/LICENSE.txt

mkdir -p $RPM_BUILD_ROOT/%{_prefix}/bin
sling_exe=$RPM_BUILD_ROOT/%{_prefix}/bin/sling
cp server/.stack-work/install/*/*/*/bin/sling ${sling_exe}
cp git-propose.sh $RPM_BUILD_ROOT/%{_prefix}/bin
cp git-propose.py $RPM_BUILD_ROOT/%{_prefix}/bin
cp git-propose $RPM_BUILD_ROOT/%{_prefix}/bin

chmod a+x ${sling_exe}

# Remove RPATH absolute references to build dir.
chrpath -d ${sling_exe}

%files
%{_prefix}/bin/sling
%{_prefix}/bin/git-propose
%{_prefix}/bin/git-propose.sh
# FIXME: Version needs to be automated here.
%{_prefix}/share/doc/sling-0.1.0.0/LICENSE.txt

%changelog
